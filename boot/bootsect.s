!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000 ! SYS_SIZE是要加载的节数(16字节为一节)。0x3000字节=196KB 编译链接后system模块的大小
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

! bootsect.s bios-启动子程序加载至 0x7c00 (31k)处,并将自己
! 移到了地址 0x90000 (576k)处,并跳转。
!
! 它然后BIOS 中断将'setup'直接加载到自己的后面(0x90200)(576.5k),
! 并将 system 加载到地址 0x10000 处。
!
! 注意! 目前的内核系统最大长度限制为(8*65536)(512k)字节,即使是在
! 将来这也应该没有问题的。我想让它保持简单明了。这样 512k 的最大内核长度应该
! 足够了,尤其是这里没有象 minix 中一样包含缓冲区高速缓冲。
!
! 加载程序已经做的够简单了,所以持续的读出错将导致死循环。只能手工重启。
! 只要可能,通过一次取取所有的扇区,加载过程可以做的很快的。
!

.globl begtext, begdata, begbss, endtext, enddata, endbss !定义了6个全局标识符
.text!文本段
begtext:
.data!数据段
begdata:
.bss!堆栈段
begbss:
.text!文本段

SETUPLEN = 4				! nr of setup-sectors setup程序的扇区数
BOOTSEG  = 0x07c0			! original address of boot-sector boot-sector的原始地址，也是段地址
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). system模块加载到0x10000，也就是64kb
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading 停止加载的段地址 0x3000 + 0x1000

! ROOT_DEV:	0x000 - same type of floppy as boot. 根文件系统设备使用与引导时同样的软驱
!		0x301 - first partition on first drive etc 根文件系统设备在第一个硬盘上的第一个分区
ROOT_DEV = 0x306

! 指定根文件系统设备是第 2 个硬盘的第 1 个分区。这是 Linux 老式的硬盘命名
! 方式,具体值的含义如下:
! 设备号=主设备号*256 + 次设备号(也即 dev_no = (major<<8) + minor )
! (主设备号:1-内存,2-磁盘,3-硬盘,4-ttyx,5-tty,6-并行口,7-非命名管道)
! 0x300 - /dev/hd0 - 代表整个第 1 个硬盘;
! 0x301 - /dev/hd1 - 第 1 个盘的第 1 个分区;
! ...
! 0x304 - /dev/hd4 - 第 1 个盘的第 4 个分区;
! 0x305 - /dev/hd5 - 代表整个第 2 个硬盘盘;
! 0x306 - /dev/hd6 - 第 2 个盘的第 1 个分区;
! ...
! 0x309 - /dev/hd9 - 第 2 个盘的第 4 个分区;

entry start !声明链接程序位置 程序从start标号开始
! 下面代码作用是将自身(bootsect)从目前段位置 0x07c0(31k)
! 移动到 0x9000(576k)处,共 256 字(512 字节),然后跳转到
! 移动后代码的 go 标号处,也即本程序的下一语句处。
start:
	mov	ax,#BOOTSEG 
	mov	ds,ax ! 把ds段寄存器置为0x07c0
	mov	ax,#INITSEG
	mov	es,ax !把es段寄存器置为0x9000
	mov	cx,#256 !计数器设置为256字
	sub	si,si ! 源地址 ds:si = 0x07c0:0x0000
	sub	di,di ! 目的地址 es:di = 0x9000:0x0000
	rep ! 重复执行 ，直到 cx == 0
	movw ! 移动一个字(word)
	jmpi	go,INITSEG ! 段内跳转 程序 从INITSEG(0x9000):go (go是偏移地址) 开始执行 
go:	mov	ax,cs ! 将ds es ss 都设置成移动后代码所在段位置
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax ! 将堆栈段sp指向0x9ff00(0x9000:0xff00)
	mov	sp,#0xFF00		! arbitrary value >>512

! sp 只要指向远大于 512 偏移(即地址 0x90400)处
! 都可以。因为从 0x90200 地址开始要放置 setup 程序,
! 而此时 setup 程序大约为 4 个扇区,因此 sp 要指向大
! 于(200 + 200 * 4 + 堆栈大小)处。

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
! 在bootsect程序块后紧根着加载setup模块的代码数据
! es已经设置指向目的段地址处0x9000。

! load_setup用途是利用 BIOS 中断 INT 0x13 将 setup 模块从磁盘第 2 个扇区
! 开始读到 0x90200 开始处,共读 4 个扇区。如果读出错,则复位驱动器,并
! 重试,没有退路。INT 0x13 的使用方法如下:
! 读扇区:
! ah = 0x02 - 读磁盘扇区到内存; al = 需要读出的扇区数量;
! ch = 磁道(柱面)号的低 8 位;
! cl = 开始扇区(0-5 位),磁道号高 2 位(6-7);
! dh = 磁头号;
! dl = 驱动器号(如果是硬盘则要置位 7);
! es:bx 指向数据缓冲区; 如果出错则 CF 标志置位。
load_setup:
	mov	dx,#0x0000		! drive 0, head 0 ! 0号驱动器，0号磁头
	mov	cx,#0x0002		! sector 2, track 0 ! 2号扇区，0号柱面
	mov	bx,#0x0200		! address = 512, in INITSEG ! 指向数据缓冲区，设置标志位
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors ! 从512字节开始，读取4个扇区的位置
	int	0x13			! read it ! 设置中断0x13 进行扇区读取
	jnc	ok_load_setup		! ok - continue ! CF=0时跳转，AX大于等于0时，跳转到ok_load_setup
	mov	dx,#0x0000 ! dx清空
	mov	ax,#0x0000		! reset the diskette ！ax 清空
	int	0x13 ! 如果不满足上面jnc继续循环执行读取扇区的操作
	j	load_setup ! 无条件跳转循环

ok_load_setup:

! 取磁盘驱动器的参数,特别是每道的扇区数量。
! 取磁盘驱动器参数 INT 0x13 调用格式和返回信息如下:
! ah = 0x08
! dl = 驱动器号(如果是硬盘则要置位 7 为 1)。
! 返回信息:
! 如果出错则 CF 置位,并且 ah = 状态码。
! ah = 0, al = 0,
! bl = 驱动器类型(AT/PS2)
! ch = 最大磁道号的低 8 位,cl = 每磁道最大扇区数(位 0-5),最大磁道号高 2 位(位 6-7)
! dh = 最大磁头数,
! dl = 驱动器数量,
! es:di - 软驱磁盘参数表

	mov	dl,#0x00 ! 清空dl
	mov	ax,#0x0800		! AH=8 is get drive parameters ! AX高位设置0x08
	int	0x13 ! 设置中断调用
	mov	ch,#0x00 ! 清零ch
	seg cs ! 返回cs的段基址
	mov	sectors,cx ! 将cx中的值复制给sectors 保存每磁道扇区数
	mov	ax,#INITSEG ! ax初始化
	mov	es,ax ! 重新改回es的值

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
