!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! setup.s 负责从 BIOS 中获取系统数据,并将这些数据放到系统内存的适当地方。
! 此时 setup.s 和 system 已经由 bootsect 引导块加载到内存中。
! 这段代码询问 bios 有关内存/磁盘/其它参数,并将这些参数放到一个
! “安全的”地方:0x90000-0x901FF,也即原来 bootsect 代码块曾经在
! 的地方,然后在被缓冲块覆盖掉之前由保护模式的 system 读取。

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way ! 原本bootsect所处的段
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536). ! system 在64k处
SETUPSEG = 0x9020	! this is the current segment ! 本程序所处在的段基址

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but... ! 重置ds的基址
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos

! BIOS 中断 0x10 的读光标功能号 ah = 0x03
! 输入:bh = 页号
! 返回:ch = 扫描开始线,cl = 扫描结束线,
! dh = 行号(0x00 是顶端),dl = 列号(0x00 是左边)。

	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.
! 光标位置保存在0x90000处，控制台初始化会使用

! Get memory size (extended mem, kB)

! 下面3句取扩展内存的大小值(KB)。
! 是调用中断 0x15,功能号 ah = 0x88
! 返回:ax = 从 0x100000(1M)处开始的扩展内存大小(KB)。
! 若出错则 CF 置位,ax = 出错码。

	mov	ah,#0x88 
	int	0x15
	mov	[2],ax ! 将扩展内存数值保存在0x90002(一个字)

! Get video-card data:

! 下面这段用于取显示卡当前显示模式。
! 调用 BIOS 中断 0x10,功能号 ah = 0x0f
! 返回:ah = 字符列数,al = 显示模式,bh = 当前显示页。
! 0x90004(1 字)存放当前页,0x90006 显示模式,0x90007 字符列数。

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

! 检查显示方式(EGA/VGA)并取参数。
! 调用 BIOS 中断 0x10,附加功能选择 -取方式信息
! 功能号:ah = 0x12,bl = 0x10
! 返回:bh = 显示状态
!(0x00 - 彩色模式,I/O 端口=0x3dX)
!(0x01 - 单色模式,I/O 端口=0x3bX)
! bl = 安装的显示内存
! (0x00 - 64k, 0x01 - 128k, 0x02 - 192k, 0x03 = 256k)
! cx = 显示卡特性参数(参见程序后的说明)。

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax ! 0x90008 = 保存的功能号数值
	mov	[10],bx ! 0x9000A = 安装的显存，0x9000B = 显示状态(彩色/单色)
	mov	[12],cx ! 0x9000C = 显卡特性参数

! Get hd0 data

! 取第一个硬盘的信息(复制硬盘参数表)。
! 第1个硬盘参数表的首地址是中断向量 0x41 的向量值，第2个硬盘
! 参数表紧接第1个表的后面,中断向量 0x46 的向量值也指向这第2个硬盘
! 的参数表首址。表的长度是16个字节(0x10)。

! 下面两段程序分别复制 BIOS 有关两个硬盘的参数表,0x90080 处存放第 1 个
! 硬盘的表,0x90090 处存放第 2 个硬盘的表。

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41] ! 取中断向量0x41的值，也就是hd0参数表中的地址ds:si
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080 ! 传输的目的地址 0x9000:0x0080 -> es:di
	mov	cx,#0x10 ! 传输0x10字节，也就是一个表的长度
	rep
	movsb

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46] ! 取中断向量0x46的值,就是hd1参数表的地址ds:si
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090 ! 传输的目的地址: 0x9000:0x0090->es:di
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

! 检查系统是否存在第 2 个硬盘,如果不存在则第 2 个表清零。
! 利用 BIOS 中断调用 0x13 的取盘类型功能。
! 功能号 ah = 0x15;
! 输入:dl = 驱动器号(0x8X 是硬盘:0x80 指第 1 个硬盘,0x81 第 2 个硬盘)
! 输出:ah = 类型码;00 --没有这个盘,CF 置位; 01 --是软驱, 02 --是软驱(或其它可移动设备),有 change-line03 --是硬盘。

	mov	ax,#0x01500 ! 功能号输入
	mov	dl,#0x81 ! 取第一个硬盘
	int	0x13 ! 中断号
	jc	no_disk1 ! 检查是否是硬盘，并跳转
	cmp	ah,#3 ! 类型码是否是硬盘
	je	is_disk1 ! 是就跳转
no_disk1:
	mov	ax,#INITSEG ! 第二个硬盘不存在，对第二个硬盘表清零
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...
! 接下来是保护模式
	cli			! no interrupts allowed !
! 此时不许中断
! first we move the system to it's rightful place

! 首先我们将 system 模块移到正确的位置。
! bootsect 引导程序是将 system 模块读入到从 0x10000(64k)开始的位置。由于当时假设
! system 模块最大长度不会超过 0x80000(512k),也即其末端不会超过内存地址 0x90000,
! 所以 bootsect 会将自己移动到 0x90000 开始的地方,并把 setup 加载到它的后面。

! 下面这段程序的用途是再把整个 system 模块移动到 0x00000 位置,即把从 0x10000 到 0x8ffff
! 内存数据块(512k),整块地向内存低端移动了 0x10000(64k)的位置。

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward

do_move:
	mov	es,ax		! destination segment es:di 为目的地址(0x0000:0x0)
	add	ax,#0x1000
	cmp	ax,#0x9000 ! 是否将从0x8000段开始的64k代码移动完毕
	jz	end_move
	mov	ds,ax		! source segment ! ds:si为源地址(0x1000:0x0)
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000 (移动0x8000字)
	rep
	movsw
	jmp	do_move ! 循环检查

! then we load the segment descriptors

! 此后,加载段描述符。
! lidt 指令用于加载中断描述符表(idt)寄存器,它的操作数是 6 个字节,0-1 字节是描述符表的
! 长度值(字节);2-5 字节是描述符表的 32 位线性基地址(首地址)
! 中断描述符表中的每一个表项(8 字节)指出发生中断时
! 需要调用的代码的信息,与中断向量有些相似,但要包含更多的信息。
! lgdt 指令用于加载全局描述符表(gdt)寄存器,其操作数格式与 lidt 指令的相同。全局描述符
! 表中的每个描述符项(8 字节)描述了保护模式下数据和代码段(块)的信息。其中包括段的
! 最大长度限制(16 位)、段的线性基址(32 位)、段的特权级、段是否在内存、读写许可以及
! 其它一些保护模式运行的标志

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax ! ds指向程序段(setup)
	lidt	idt_48		! load idt with 0,0
    ! 加载中断描述符表(idt)寄存器,idt_48 是 6 字节操作数的位置
    ! 前 2 字节表示 idt 表的限长,后 4 字节表示 idt 表所处的基地址。
	lgdt	gdt_48		! load gdt with whatever appropriate
    ! 加载全局描述符表(gdt)寄存器,gdt_48 是 6 字节操作数的位置

! that was painless, now we enable A20
! 开启A20地址线
	call	empty_8042 ! 等待输入缓冲器空，只有当输入缓冲器为空时才可以对其进行写命令。
	mov	al,#0xD1		! command write 0xD1 命令码-表示要写数据到8042 的 P2 端口。
                        ! P2 端口的位 1 用于 A20 线的选通
	out	#0x64,al ! 数据要写到 0x60 端口。

	call	empty_8042 ! 等待输入缓冲器空,看命令是否被接受。
	mov	al,#0xDF		! A20 on 选通 A20 地址线的参数。
	out	#0x60,al ! 数据写到0x60
	call	empty_8042 ! 输入缓冲器为空,则表示 A20 线已经选通

! 现在必须重新对中断进行编程
! 将它们放在正好处于 intel 保留的硬件中断后面,在 int 0x20-0x2F
! PC 机的 bios 将中断放在了 0x08-0x0f,这些中断也被用于内部硬件中断。
! 所以必须重新对8259中断控制器进行编程

	mov	al,#0x11		! initialization sequence 
    ! 0x11 表示初始化命令开始,
    ! 是 ICW1 命令字,表示边沿触发、多片 8259
    ! 级连、最后要发送 ICW4 命令字。

    out	#0x20,al		! send it to 8259A-1 ! 发送到8259A主芯片
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
    ! $ 表示当前指令的地址,
    ! 两条跳转指令,跳到下一条指令,起延时作用。
	
    out	#0xA0,al		! and to 8259A-2 ! 再发送到主芯片
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
    ! 送 主 芯片 ICW2 命令字,起始中断号,要送奇地址。
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
    ! 送 从 芯片 ICW2 命令字,起始中断号。
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
    ! 送主芯片 ICW3 命令字,主芯片的 IR2 连从芯片 INT。
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
    ! 送从芯片 ICW3 命令字,表示从芯片的 INT 连到主芯片的 IR2 引脚上。
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
    ! 送两个芯片的 ICW4 命令字。8086 模式;普通 EOI 方式,需发送指令来复位。初始化结束,芯片就绪。
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
    ! 当前屏蔽所有中断请求。
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.

! 这里设置进入 32 位保护模式运行。首先加载机器状态字(lmsw - Load Machine Status Word),也称
! 控制寄存器 CR0,其比特位 0 置 1 将导致 CPU 工作在保护模式。

	mov	ax,#0x0001	! protected mode (PE) bit ! 保护模式bit
	lmsw	ax		! This is it! ! 加载状态字
	jmpi	0,8		! jmp offset 0 of segment 8 (cs) ! 跳转至 cs 段 8,偏移 0 处。

! 已经将 system 模块移动到 0x00000 开始的地方,所以这里的偏移地址是 0。这里的段
! 值的 8 已经是保护模式下的段选择符,用于选择描述符表和描述符表项以及所要求的特权级。

! 段选择符长度为 16 位(2 字节);位 0-1 表示请求的特权级 0-3,linux 操作系统只
! 用到两级:0 级(系统级)和 3 级(用户级);位 2 用于选择全局描述符表(0)还是局部描
! 述符表(1);位 3-15 是描述符表项的索引,指出选择第几项描述符。

! 所以段选择符8(0b0000,0000,0000,1000)表示请求特权级 0、使用全局描述符表中的第 1 项,该项指出
! 代码的基地址是 0,因此这里的跳转指令就会去执行 system 中的代码。


! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.

! 下面这个子程序检查键盘命令队列是否为空。这里不使用超时方法 - 如果这里死机,
! 则说明 PC 机有问题,我们就没有办法再处理下去了。
! 只有当输入缓冲器为空时(状态寄存器位 2 = 0)才可以对其进行写命令

empty_8042:
	.word	0x00eb,0x00eb ! 延时操作
	in	al,#0x64	! 8042 status port ! 读AT键盘控制器状态寄存器
	test	al,#2		! is input buffer full? ! 测试位2，输入缓冲器满判断
	jnz	empty_8042	! yes - loop ! 循环判断
	ret

gdt:

! 全局描述符表开始处。描述符表由多个 8 字节长的描述符项组成。
! 这里给出了 3 个描述符项。第 1 项无用,但须存在。
! 第 2 项是系统代码段描述符,第 3 项是系统数据段描述符。

	.word	0,0,0,0		! dummy ! 第 1 个描述符,不用。
! 这里在 gdt 表中的偏移量为 0x08,当加载代码段寄存器(段选择符)时,使用的是这个偏移值。

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386
! 这里在 gdt 表中的偏移量是 0x10,当加载数据段寄存器(如 ds 等)时,使用的是这个偏移值。

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
    ! 全局表长度为 2k 字节,因为每 8 字节组成一个段描述符项
    ! 所以表中共可有 256 项。

	.word	512+gdt,0x9	! gdt base = 0X9xxxx
	
.text
endtext:
.data
enddata:
.bss
endbss:


setup 程序的作用主要是利用 ROM BIOS 中断读取机器系统数据,并将这些数据保存到 0x90000 开始的
位置(覆盖掉了 bootsect 程序所在的地方),这些参数将被内核中相关程序使用。

然后 setup 程序将 system 模块从 0x10000-0x8ffff(当时认为内核系统模块 system 的长度不会超过
此值:512KB)整块向下移动到内存绝对地址 0x00000 处。接下来加载中断描述符表寄存器(idtr)和全局
描述符表寄存器(gdtr),开启 A20 地址线,重新设置两个中断控制芯片 8259A,将硬件中断号重新设置为
0x20 - 0x2f。最后设置 CPU 的控制寄存器 CR0(也称机器状态字)
,从而进入 32 位保护模式运行,并跳转到位于 system 模块最前面部分的 head.s 程序继续运行。
