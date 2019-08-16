/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
# 32 位启动代码是从绝对地址 0x00000000 开始的,这里也同样是页目录将存在的地方,
# 这里的启动代码将被页目录覆盖掉。
# 对于AT&T汇编来说,每个直接数要以'$'开始,否则是表示地址。
# 每个寄存器名都要以'%'开头,eax 表示是 32 位的 ax 寄存器。

.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area
_pg_dir: # 页目录会存放在这里
startup_32:
	movl $0x10,%eax
    #这里已经处于 32 位运行模式,因此这里的$0x10 并不是把地址 0x10 装入各个段寄存器,
    #它现在其实是全局段描述符表中的偏移值,或者更正确地说是一个描述符表项的选择符。
    #这里$0x10 的含义是请求特权级 0(位 0-1=0)、选择全局描述符表(位 2=0)、选择表中第 1 项(位 3-15=1)。
    #正好指向表中的数据段描述符项。
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp # 表示_stack_start->ss:esp。
        #_stack_start是编译程序为程序自动生成的存有堆栈信息地方。
	call setup_idt # 调用设置中断描述符表子程序。
	call setup_gdt # 调用设置全局描述符表子程序。
	movl $0x10,%eax		# reload all the segment registers 重新加载所有段寄存器
	mov %ax,%ds		# 在改变过全局描述符表 gdt 之后,CS 代码段寄存器已经在 setup_gdt 中重新加载过了。
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp
    # 下5行行用于测试 A20 地址线是否已经开启。采用的方法是向内存地址 0x000000 处写入任意
    # 一个数值,然后看内存地址 0x100000(1M)处是否也是这个数值。如果一直相同的话,就一直
    # 比较下去,也即死循环、死机。表示地址 A20 线没有选通,结果不能对 1M 以上内存寻址。
	xorl %eax,%eax
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b # 1b'表示向后(backward)跳转到标号1。若是'5f'则表示向前(forward)跳转到标号5。
/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
/*
* 在下面这段程序中,486 应该将位 16 置位,以检查在超级用户模式下的写保护,
* 此后"verify_area()"调用中就不需要了。486 的用户通常也会想将 NE(#5)置位,以便
* 对数学协处理器的出错使用 int 16。
*/
# 下面这段程序用于检查数学协处理器芯片是否存在。方法是修改控制寄存器 CR0,在
# 假设存在协处理器的情况下执行一个协处理器指令,如果出错的话则说明协处理器芯片不存在,
# 需要设置 CR0 中的协处理器仿真位 EM(位 2),并复位协处理器存在标志 MP(位 1)。
	movl %cr0,%eax		# check math chip 设置cr0仿真位
	andl $0x80000011,%eax	# Save PG,PE,ET 保存数值
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP 设置协处理器 
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables ! 跳转到函数

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
! 依赖于ET标志的正确性来检测287/387的存在
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax ! 如果存在的则向前跳转到标号 1 处,否则改写 cr0。
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
# 这里".align 2"的含义是指存储边界对齐调整。"2"表示调整到地址最后2位为零,
# 即按 4 字节方式对齐内存地址。
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */ # 287协处理器码
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */

/*
* 下面这段是设置中断描述符表子程序 setup_idt
* 将中断描述符表 idt 设置成具有 256 个项,并都指向 ignore_int 中断门。然后加载中断
* 描述符表寄存器(用 lidt 指令)。真正实用的中断门以后再安装。其它地方一切都正常时再开启中断。
* 该子程序将会被页表覆盖掉。
*/
# 中断描述符表中的项虽然也是 8 字节组成,但其格式与全局表中的不同,被称为门描述符
# (Gate Descriptor)。它的 0-1,6-7 字节是偏移量,2-3 字节是选择符,4-5 字节是一些标志。

setup_idt:
	lea ignore_int,%edx # 将 ignore_int 的有效地址(偏移值)值edx->寄存器
	movl $0x00080000,%eax # 将选择符 0x0008 置入 eax 的高 16 位中。
	movw %dx,%ax		/* selector = 0x0008 = cs */
                        # 偏移值的低 16 位置入 eax 的低 16 位中。此时 eax 含有
                        # 门描述符低 4 字节的值。
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */
                        # 此时 edx 含有门描述符高 4 字节的值。
	lea _idt,%edi # _idt 是中断描述符表的地址。
	mov $256,%ecx # 计数器

rp_sidt:
	movl %eax,(%edi) # 将哑中断门描述符存入表中。
	movl %edx,4(%edi)
	addl $8,%edi # edi 指向表中下一项。
	dec %ecx # 自减
	jne rp_sidt # 循环 条件为ce=0
	lidt idt_descr # 加载中断描述符表寄存器值。
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $_main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 2
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
