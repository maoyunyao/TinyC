#include "stdio.h"
#include "stdlib.h"
#include "constvar.h"

extern TERMINAL nextToken();
extern void renewLex();
static int match (int t);
static int strcompare(char *sstr, char *tstr);	// 比较两个串
static IDTABLE* InstallID();					// 在符号表中为curtoken_str建立一个条目
static IDTABLE* LookupID();						// 在符号表中查找curtoken_str
static void FreeExit();
static int cast2int(EXPVAL exp);				// 将exp的值转换为int类型
static char cast2char(EXPVAL exp);				// 将exp的值转换为char类型
static int Prod_FUNC();
static int Prod_S();
static int Prod_D();
static int Prod_L(int type);
static int Prod_T();
static int Prod_A();
static EXPVAL Prod_B();
static EXPVAL Prod_B1(EXPVAL bval);
static EXPVAL Prod_TB();
static EXPVAL Prod_TB1(EXPVAL bval);
static EXPVAL Prod_FB();
static EXPVAL Prod_E();
static EXPVAL Prod_E1(EXPVAL val);
static EXPVAL Prod_TE();
static EXPVAL Prod_TE1(EXPVAL val);
static EXPVAL Prod_F();

extern FILE *sFile;
static TERMINAL lookahead;
static int curtoken_num;

/*
@ Yunyao Mao
@ myy2016@mail.ustc.edu.cn
@ 2019.5.12
@ 实现(完善) char 型变量处理
*/
//start
static char curtoken_char;
//end

static char curtoken_str[MAXTOKENLEN];
static IDTABLE *IDTHead=NULL;
static int run_status=1;						// 0；程序不执行；1:程序正常执行；2:跳过当前结构后继续执行

/*
@ Yunyao Mao
@ myy2016@mail.ustc.edu.cn
@ 2019.5.12
@ 实现 continue & break
*/
//start
static int ctn_brk_status=1;					// 1:循环体内部的语句正常执行;
												// 2:continue,停止执行后续的循环体内部语句直到下一次循环开始;
												// 3:break,停止执行后续循环内语句并跳出循环;
//end

void SyntaxAnalysis()
{
#if defined(AnaTypeLex)
//testing lexical analysis
	TERMINAL token;
	token=nextToken();
	while (token.token!=ERR)
	{	if (token.token==SYN_NUM)
			printf("LEX: %d,%d\n",token.token,token.tokenVal.number);
		else if (token.token==SYN_ID)
			printf("LEX: %d,%s\n",token.token,token.tokenVal.str);
		else
			printf("LEX: %d\n",token.token);
		token=nextToken();
	}
#else
//syntax analysis
	lookahead=nextToken();
	if (Prod_FUNC()==ERR)
		printf("PROGRAM HALT!\n");
	FreeExit();

#endif
}

static int match (int t)
{
	char *p,*q;
	if (lookahead.token == t)														// 判断类型是否匹配
	{	if (t==SYN_NUM)
			curtoken_num=lookahead.tokenVal.number;
		else if (t==SYN_ID)
			for (p=lookahead.tokenVal.str,q=curtoken_str;(*q=*p)!='\0';p++,q++);
		/*
		@ Yunyao Mao
		@ myy2016@mail.ustc.edu.cn
		@ 2019.5.12
		@ 实现(完善) char 型变量处理
		*/
		//start
		else if (t==SYN_CHAR)
			curtoken_char=lookahead.tokenVal.str[0];
		//end

		lookahead = nextToken();													// 读取下一个关键字
	}
	else
		FreeExit();
	return(0);
}

static int strcompare(char *sstr, char *tstr)
{
	while (*sstr==*tstr && *sstr!='\0') { sstr++; tstr++; }
	if (*sstr=='\0' && *tstr=='\0')	return(0);
	else return(ERR);
}

static IDTABLE* InstallID()
{
	IDTABLE *p,*q;
	char *a,*b;
	p=IDTHead; q=NULL;
	while (p!=NULL && strcompare(curtoken_str,p->name)==ERR)	// 扫描符号表查看符号是否已经存在
	{
		q=p;
		p=p->next;
	}
	if (p!=NULL)
		return(NULL);
	else
	{
		p=(IDTABLE*)malloc(sizeof(IDTABLE));
		if (q==NULL)
			IDTHead=p;
		else
			q->next=p;
		p->next=NULL;
		for (a=curtoken_str,b=p->name;(*b=*a)!='\0';a++,b++);	// 存储新关键字
		return(p);
	}
}

static IDTABLE* LookupID()
{
	IDTABLE *p;
	p=IDTHead;
	while (p!=NULL && strcompare(curtoken_str,p->name)==ERR)
		p=p->next;
	if (p==NULL)
		return(NULL);
	else
		return(p);
}

static void FreeExit()
{
	IDTABLE *p,*q;
	//释放链表空间
	p=IDTHead;
	while ((q=p)!=NULL)
	{	p=p->next;
		#if defined(AnaTypeSyn)
		printf("NAME:%s, TYPE:%d, ",q->name,q->type);
		if (q->type==ID_INT)
			printf("VALUE:%d\n",q->val.intval);
		else if (q->type==ID_CHAR)
			printf("VALUE:%c\n",q->val.charval);
		else
			printf("\n");
		#endif
		free(q);
	}
	exit(0);
}

static int cast2int(EXPVAL exp)
{
	if (exp.type==ID_INT)
		return(exp.val.intval);
	else if (exp.type==ID_CHAR)
		return((int)(exp.val.charval));
}

static char cast2char(EXPVAL exp)
{
	if (exp.type==ID_INT)
		return((char)(exp.val.intval));
	else if (exp.type==ID_CHAR)
		return(exp.val.charval);
}

static int Prod_FUNC()
{
	IDTABLE *p;
	match(SYN_ID);
	if (strcompare(curtoken_str,"main")==ERR) FreeExit();
	p=InstallID();
	p->type=ID_FUN;
	#if defined(AnaTypeSyn)
	printf("SYN: FUNC-->main() {S}\n");
	#endif
	match(SYN_PAREN_L);
	match(SYN_PAREN_R);
	match(SYN_BRACE_L);
	Prod_S();
	match(SYN_BRACE_R);
	return(0);
}

static int Prod_S()
{
	long file_index;
	/*
	@ Yunyao Mao
	@ myy2016@mail.ustc.edu.cn
	@ 2019.5.12
	@ 实现 continue & break
	*/
	//start
	int record_run_status;		// 用于记录当前的run_status
	int record_ctn_brk_status;	// 用于记录当前的ctn_brk_status
	int permission;				// 运行许可(run_status 和 ctn_brk_status 同时为1时其值为1)
	//end
	EXPVAL exp;
	EXPVAL bval;
	if (lookahead.token==SYN_INT || lookahead.token==SYN_CHAR)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->D S\n");
		#endif
		Prod_D();
		Prod_S();
	}
	else if (lookahead.token==SYN_ID)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->A S\n");
		#endif
		Prod_A();
		Prod_S();
	}
	else if (lookahead.token==SYN_SHOW)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->show(E); S\n");
		#endif
		permission=run_status==1 && ctn_brk_status==1;
		match(SYN_SHOW);
		match(SYN_PAREN_L);
		exp=Prod_E();
		match(SYN_PAREN_R);
		match(SYN_SEMIC);
		if (permission==1)
			if (exp.type==ID_INT)
				printf("%d\n",exp.val.intval);
			else if (exp.type==ID_CHAR)
				printf("%c\n",exp.val.charval);
		Prod_S();
	}
	else if (lookahead.token==SYN_IF)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->if (B) {S} [else {S}] S");
		#endif
		record_run_status=run_status;
		permission=run_status==1 && ctn_brk_status==1;
		match(SYN_IF);
		match(SYN_PAREN_L);
		bval=Prod_B();
		match(SYN_PAREN_R);
		if (permission==1 && cast2int(bval)==0) run_status=2;
		match(SYN_BRACE_L);
		Prod_S();
		match(SYN_BRACE_R);
		if (lookahead.token==SYN_ELSE)
		{
			match(SYN_ELSE);
			if (run_status==1 && permission==1) run_status=2;
			else if (run_status==2 && permission==1) run_status=1;
			match(SYN_BRACE_L);
			Prod_S();
			match(SYN_BRACE_R);
		}
		/*
		@ Yunyao Mao
		@ myy2016@mail.ustc.edu.cn
		@ 2019.5.12
		@ BUG_5 & continue & break
		*/
		//start
		run_status=record_run_status;
		// end
		Prod_S();
	}
	else if (lookahead.token==SYN_WHILE)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->while(B) {S} S\n"); modify 
		#endif
		/*
		@ Yunyao Mao
		@ myy2016@mail.ustc.edu.cn
		@ 2019.5.12
		@ BUG_5 & continue & break
		*/
		//start
		record_run_status=run_status;							// 记录run_status
		record_ctn_brk_status=ctn_brk_status;					// 记录ctn_brk_status
		permission=run_status==1 && ctn_brk_status==1;
		do
		{
			match(SYN_WHILE);
			file_index=ftell(sFile)-6;
			match(SYN_PAREN_L);
			bval=Prod_B();
			match(SYN_PAREN_R);
			if (permission==1 && cast2int(bval)==0) run_status=2;
			match(SYN_BRACE_L);
			Prod_S();
			match(SYN_BRACE_R);
			if (permission==1 && ctn_brk_status==2)
			{
				ctn_brk_status=1;
				run_status=1;
			}
			else if(permission==1 && ctn_brk_status==3)
			{
				ctn_brk_status=1;
				run_status=2;
			}
			if (permission==1 && run_status==1)
			{	
				fseek(sFile,file_index,SEEK_SET);
				renewLex();
				lookahead = nextToken();
			}
		}while(run_status == 1 && permission==1);				// 此处必须使用do-while而非while!
		run_status=record_run_status;							// 恢复run_status
		ctn_brk_status=record_ctn_brk_status;					// 恢复ctn_brk_status
		// end
		Prod_S();
	}
	else if (lookahead.token==SYN_CTN)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->continue; S");
		#endif
		permission=run_status==1 && ctn_brk_status==1;
		match(SYN_CTN);
		match(SYN_SEMIC);
		if(permission==1) ctn_brk_status=2;
		Prod_S();
	}
	else if (lookahead.token==SYN_BRK)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S-->break; S");
		#endif
		permission=run_status==1 && ctn_brk_status==1;
		match(SYN_BRK);
		match(SYN_SEMIC);
		if(permission==1) ctn_brk_status=3;
		Prod_S();
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: S--> \n");
		#endif
	}
	return(0);
}

static int Prod_D()
{
	int type;
	IDTABLE *p;
	EXPVAL exp;
	#if defined(AnaTypeSyn)
	printf("SYN: D-->T id [=E] L;\n");
	#endif
	type=Prod_T();
	match(SYN_ID);
	p=InstallID();
	p->type=type;
	if (lookahead.token==SYN_SET)
	{
		match(SYN_SET);
		exp=Prod_E();
		if (run_status==1 && ctn_brk_status==1)
		{	if (type==ID_INT)
				p->val.intval=cast2int(exp);
			else if (type==ID_CHAR)
				p->val.charval=cast2char(exp);
		}
	}
	Prod_L(type);
	match(SYN_SEMIC);
	return(0);
}

static int Prod_L(int type)
{
	IDTABLE *p;
	EXPVAL exp;
	if (lookahead.token==SYN_COMMA)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: L-->, id [=E] L\n");
		#endif
		match(SYN_COMMA);
		match(SYN_ID);
		p=InstallID();
		p->type=type;
		if (lookahead.token==SYN_SET)
		{
			match(SYN_SET);
			exp=Prod_E();
			if (run_status==1 && ctn_brk_status==1)
			{	if (type==ID_INT)
					p->val.intval=cast2int(exp);
				else if (type==ID_CHAR)
					p->val.charval=cast2char(exp);
			}
		}
		Prod_L(type);
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: L--> \n");
		#endif
	}
	return(0);
}

static int Prod_T()
{
	if (lookahead.token==SYN_INT)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: T-->int\n");
		#endif
		match(SYN_INT);
		return(ID_INT);
	}
	else if (lookahead.token==SYN_CHAR)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: T-->char\n");
		#endif
		match(SYN_CHAR);
		return(ID_CHAR);
	}
	else
		FreeExit();
	return(0);
}

static int Prod_A()
{
	IDTABLE *p;
	EXPVAL exp;
	#if defined(AnaTypeSyn)
	printf("SYN: A-->id=E;\n");
	#endif
	match(SYN_ID);
	p=LookupID();
	match(SYN_SET);
	exp=Prod_E();
	match(SYN_SEMIC);
	if (run_status==1 && ctn_brk_status==1)
	{	if (p->type==ID_INT)
			p->val.intval=cast2int(exp);
		else if (p->type==ID_CHAR)
			p->val.charval=cast2char(exp);
	}
	return(0);
}

static EXPVAL Prod_B()
{
	EXPVAL bval1,bval2;
	#if defined(AnaTypeSyn)
	printf("SYN: B-->id=TB B1\n");
	#endif
	bval1=Prod_TB();
	bval2=Prod_B1(bval1);
	return(bval2);
}

static EXPVAL Prod_B1(EXPVAL bval1)
{
	EXPVAL bval2;
	if (lookahead.token==SYN_OR)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: B1-->|| B1\n");
		#endif
		match(SYN_OR);
		bval2=Prod_TB();
		if(bval1.type==ID_CHAR)
		{
			bval1.type=ID_INT;
			bval1.val.intval=cast2int(bval1);
		}
		if(bval2.type==ID_CHAR)
		{
			bval2.type=ID_INT;
			bval2.val.intval=cast2int(bval2);
		}
		bval1.val.intval=(run_status==1 && (bval1.val.intval!=0 || bval2.val.intval!=0) && ctn_brk_status==1) ? 1 : 0;
		bval2=Prod_B1(bval1);
		return(bval2);
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: B1--> \n");
		#endif
		return(bval1);
	}
}

static EXPVAL Prod_TB()
{
	EXPVAL bval1,bval2;
	#if defined(AnaTypeSyn)
	printf("SYN: TB-->FB TB1\n");
	#endif
	bval1=Prod_FB();
	bval2=Prod_TB1(bval1);
	return(bval2);
}

static EXPVAL Prod_TB1(EXPVAL bval1)
{
	EXPVAL bval2;
	if (lookahead.token==SYN_AND)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: TB1-->&& TB1\n");
		#endif
		match(SYN_AND);
		bval2=Prod_FB();			// BUG_3 原本为 bval1=Prod_FB(), 猜测为笔误;
		if(bval1.type==ID_CHAR)
		{
			bval1.type=ID_INT;
			bval1.val.intval=cast2int(bval1);
		}
		if(bval2.type==ID_CHAR)
		{
			bval2.type=ID_INT;
			bval2.val.intval=cast2int(bval2);
		}
		bval1.val.intval=(run_status==1 && (bval1.val.intval!=0 && bval2.val.intval!=0) && ctn_brk_status==1) ? 1 : 0;
		bval2=Prod_TB1(bval1);
		return(bval2);
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: TB1--> \n");
		#endif
		return(bval1);
	}
}

static EXPVAL Prod_FB()
{
	EXPVAL bval;
	EXPVAL val1,val2;
	EXPVAL retval;
	int ival1,ival2;
	if (lookahead.token==SYN_NOT)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: FB-->!B\n");
		#endif
		match(SYN_NOT);
		bval=Prod_B();
		if(run_status==1 && cast2int(bval)==0 && ctn_brk_status==1){
			retval.type=ID_INT;
			retval.val.intval=1;
			return retval;
		}
	}
	else if (lookahead.token==SYN_TRUE)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: FB-->TRUE\n");
		#endif
		match(SYN_TRUE);
		retval.type=ID_INT;
		retval.val.intval=0;
		if(run_status==1 && ctn_brk_status==1){
			retval.val.intval=1;
		}
		return(retval);
	}
	else if (lookahead.token==SYN_FALSE)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: FB-->FALSE\n");
		#endif
		match(SYN_FALSE);
		retval.type=ID_INT;
		retval.val.intval=0;
		if(run_status==1 && ctn_brk_status==1){
			retval.val.intval=0;
		}
		return(retval);
	}
	else if (lookahead.token==SYN_ID || lookahead.token==SYN_NUM || lookahead.token==SYN_CHAR || lookahead.token==SYN_PAREN_L)
	{
		val1=Prod_E();
		if (run_status==1 && ctn_brk_status==1) ival1=cast2int(val1);
		if (lookahead.token==SYN_LT)
		{	
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E<E\n");
			#endif
			match(SYN_LT);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	
				ival2=cast2int(val2);
				retval.val.intval=(ival1<ival2);
			}
			return(retval);
		}
		else if (lookahead.token==SYN_LE)
		{
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E<=E\n");
			#endif
			match(SYN_LE);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	ival2=cast2int(val2);
				retval.val.intval=(ival1<=ival2);
			}
			return retval;
		}
		else if (lookahead.token==SYN_GT)
		{
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E>E\n");
			#endif
			match(SYN_GT);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	ival2=cast2int(val2);
				retval.val.intval=(ival1>ival2);
			}
			return retval;
		}
		else if (lookahead.token==SYN_GE)
		{
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E>=E\n");
			#endif
			match(SYN_GE);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	ival2=cast2int(val2);
				retval.val.intval=(ival1>=ival2);
			}
			return retval;
		}
		else if (lookahead.token==SYN_EQ)
		{
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E==E\n");
			#endif
			match(SYN_EQ);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	ival2=cast2int(val2);
				retval.val.intval=(ival1==ival2);
			}
			return retval;
		}
		else if (lookahead.token==SYN_NE)
		{
			#if defined(AnaTypeSyn)
			printf("SYN: FB-->E!=E\n");
			#endif
			match(SYN_NE);
			val2=Prod_E();
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
			{	ival2=cast2int(val2);
				retval.val.intval=(ival1!=ival2);
			}
			return retval;
		}
		else
		{
			retval.type=ID_INT;
			retval.val.intval=0;
			if (run_status==1 && ctn_brk_status==1)
				return(val1);
			else
				return(retval);
		}

	}
	else
	{	
		FreeExit();
		retval.type=ID_INT;
		retval.val.intval=0;
		return(retval);
	}
}

static EXPVAL Prod_E()
{
	EXPVAL val1,val2;
	#if defined(AnaTypeSyn)
	printf("SYN: E-->TE E1\n");
	#endif
	val1=Prod_TE();
	val2=Prod_E1(val1);
	return(val2);
}

static EXPVAL Prod_E1(EXPVAL val1)
{
	EXPVAL val2,val;
	int i1,i2;
	char c1,c2;
	if (lookahead.token==SYN_ADD)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: E1-->+TE E1\n");
		#endif
		match(SYN_ADD);
		val2=Prod_TE();
		if (run_status==1 && ctn_brk_status==1)
			if (val1.type==ID_INT || val2.type==ID_INT)
			{
				val.type=ID_INT;
				i1=cast2int(val1);
				i2=cast2int(val2);
				val.val.intval=i1+i2;
			}
			else
			{
				val.type=ID_CHAR;
				c1=cast2char(val1);
				c2=cast2char(val2);
				val.val.charval=c1+c2;
			}
		val=Prod_E1(val);
	}
	else if (lookahead.token==SYN_SUB)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: E1-->-TE E1\n");
		#endif
		match(SYN_SUB);
		val2=Prod_TE();
		if (run_status==1 && ctn_brk_status==1)
			if (val1.type==ID_INT || val2.type==ID_INT)
			{
				val.type=ID_INT;
				i1=cast2int(val1);
				i2=cast2int(val2);
				val.val.intval=i1-i2;
			}
			else
			{
				val.type=ID_CHAR;
				c1=cast2char(val1);
				c2=cast2char(val2);
				val.val.charval=c1-c2;
			}
		val=Prod_E1(val);
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: E1--> \n");
		#endif
		val=val1;
	}
	return(val);
}

static EXPVAL Prod_TE()
{
	EXPVAL val1,val2;
	#if defined(AnaTypeSyn)
	printf("SYN: TE-->F TE1\n");
	#endif
	val1=Prod_F();
	val2=Prod_TE1(val1);
	return(val2);
}

static EXPVAL Prod_TE1(EXPVAL val1)
{
	EXPVAL val2,val;
	int i1,i2;
	char c1,c2;
	if (lookahead.token==SYN_MUL)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: TE1-->*F TE1\n");
		#endif
		match(SYN_MUL);
		val2=Prod_F();
		if (run_status==1 && ctn_brk_status==1)
			if (val1.type==ID_INT || val2.type==ID_INT)
			{
				val.type=ID_INT;
				i1=cast2int(val1);
				i2=cast2int(val2);
				val.val.intval=i1*i2;
			}
			else
			{
				val.type=ID_CHAR;
				c1=cast2char(val1);
				c2=cast2char(val2);
				val.val.charval=c1*c2;
			}
		val=Prod_TE1(val);
	}
	else if (lookahead.token==SYN_DIV)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: TE1-->/F TE1\n");
		#endif
		match(SYN_DIV);
		val2=Prod_F();
		if (run_status==1 && ctn_brk_status==1)
			if (val1.type==ID_INT || val2.type==ID_INT)
			{
				val.type=ID_INT;
				i1=cast2int(val1);
				i2=cast2int(val2);
				val.val.intval=i1/i2;
			}
			else
			{
				val.type=ID_CHAR;
				c1=cast2char(val1);
				c2=cast2char(val2);
				val.val.charval=c1/c2;
			}
		val=Prod_TE1(val);
	}
	else
	{
		#if defined(AnaTypeSyn)
		printf("SYN: TE1--> \n");
		#endif
		val=val1;
	}
	return(val);
}

static EXPVAL Prod_F()
{
	EXPVAL val;
	static IDTABLE *p;
	if (lookahead.token==SYN_NUM)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: F-->num\n");
		#endif
		match(SYN_NUM);
		val.type=ID_INT;
		val.val.intval=curtoken_num;
	}
	else if (lookahead.token==SYN_ID)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: F-->id\n");
		#endif
		match(SYN_ID);
		p=LookupID();
		val.type=p->type;
		val.val=p->val;
	}
	else if (lookahead.token==SYN_PAREN_L)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: F-->(E)\n");
		#endif
		match(SYN_PAREN_L);

		/*
		@ Yunyao Mao
		@ myy2016@mail.ustc.edu.cn
		@ 2019.5.12
		@ 实现(完善) char 型变量处理
		*/
		//start
		val=Prod_B();
		//end
		match(SYN_PAREN_R);
	}
	/*
	@ Yunyao Mao
	@ myy2016@mail.ustc.edu.cn
	@ 2019.5.12
	@ 实现(完善) char 型变量处理
	*/
	//start
	else if (lookahead.token==SYN_CHAR)
	{
		#if defined(AnaTypeSyn)
		printf("SYN: F-->char\n");
		#endif
		match(SYN_CHAR);
		val.type=ID_CHAR;
		val.val.charval=curtoken_char;
	}
	//end
	else
		FreeExit();
	return(val);
}
