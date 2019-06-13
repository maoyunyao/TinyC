#include "stdio.h"
#include "constvar.h"

#define LEX_RELOOP	0
#define LEX_DELIM	1
#define LEX_MUL		2
#define LEX_DIV		3
#define LEX_ADDMIN	4
#define LEX_DIGIT	5
#define LEX_LETTER_	6
#define LEX_SYMBOL	7
/*
@ Yunyao Mao
@ myy2016@mail.ustc.edu.cn
@ 2019.5.12
@ 实现(完善) char 型变量处理
*/
//start
#define LEX_CHAR    8
//end

static char ReadAChar();
static int FoundRELOOP();
static int STR2INT();
static int FoundKeyword();
static int strcompare(char *sstr, char*tstr);

extern FILE *sFile;
static char prebuf=0;				//buffer to store the pre-read character
static char tokenStr[MAXTOKENLEN];	//token buffer
static int tokenLen;

TERMINAL nextToken()
{
	TERMINAL token;
	int state=0;
	char c,d;
	token.token=ERR;
	token.tokenVal.number=0;
	tokenLen=0;
	for (c=ReadAChar(sFile);c!=0;c=ReadAChar(sFile))
	{	tokenStr[tokenLen++]=c;
		if (tokenLen>=MAXTOKENLEN)
		{	printf("Token is too long!\n");
			break;
		}
		if (feof(sFile))
			state=LexTable[state][LEX_DELIM];
		else if (c=='<' || c=='>' || c=='=' || c=='!' || c=='&' || c=='|')
			state=LexTable[state][LEX_RELOOP];
		else if (c==' ' || c=='\t' || c=='\n')
			state=LexTable[state][LEX_DELIM];
		else if (c=='*')
			state=LexTable[state][LEX_MUL];
		else if (c=='/')
			state=LexTable[state][LEX_DIV];
		else if (c=='+' || c=='-')
			state=LexTable[state][LEX_ADDMIN];
		else if (c>='0' && c<='9')
			state=LexTable[state][LEX_DIGIT];
		else if ((c>='a' && c<='z')||(c>='A' && c<='Z')||(c=='_'))
			state=LexTable[state][LEX_LETTER_];
		else if (c=='(' || c==')' || c=='{' || c=='}' || c==',' || c==';')
			state=LexTable[state][LEX_SYMBOL];
		/*
		@ Yunyao Mao
		@ myy2016@mail.ustc.edu.cn
		@ 2019.5.12
		@ 实现(完善) char 型变量处理
		*/
		//start
		else if (c=='\'')
			state=LexTable[state][LEX_CHAR];
		//end
		else
		{	printf("Unknown symbol: %c\n",c);
			break;
		}
		if (state<100) continue;
		if (state>100 && state<200)
		{	prebuf=c;
			tokenLen--;
		}
		switch (state)
		{	case 101: token.token=FoundRELOOP();
					  break;
			case 102: token.token=SYN_DIV;
					  break;
			case 103: if (tokenStr[0]=='+') token.token=SYN_ADD;
					  else token.token=SYN_SUB;
					  break;
			case 104: token.token=SYN_NUM;
					  token.tokenVal.number=STR2INT();
					  break;
			case 105: tokenStr[tokenLen]='\0';
					  token.token=FoundKeyword();
					  token.tokenVal.str=tokenStr;
					  break;
			case 201: if (feof(sFile))
					  {//	  printf("Meet file end!\n");
						  token.token=-1;
						  break;
					  }
					  state=0; tokenLen=0;
					  continue;
			case 202: c=ReadAChar(sFile);
					  while (!feof(sFile) && ((d=ReadAChar(sFile))!='/' || c!='*'))
						  c=d;
					  state=0; tokenLen=0;
					  continue;
			case 203: while ((c=ReadAChar(sFile))!='\n' && (!feof(sFile)));
					  state=0; tokenLen=0;
					  continue;
			case 204: token.token=SYN_MUL;
					  break;
			case 205: if (tokenStr[0]=='(') token.token=SYN_PAREN_L;
					  else if (tokenStr[0]==')') token.token=SYN_PAREN_R;
					  else if (tokenStr[0]=='{') token.token=SYN_BRACE_L;
					  else if (tokenStr[0]=='}') token.token=SYN_BRACE_R;
					  else if (tokenStr[0]==',') token.token=SYN_COMMA;
					  else if (tokenStr[0]==';') token.token=SYN_SEMIC;
					  break;
			/*
			@ Yunyao Mao
			@ myy2016@mail.ustc.edu.cn
			@ 2019.5.12
			@ 实现(完善) char 型变量处理
			*/
			//start
			case 206: 
					 c=ReadAChar(sFile);
					 tokenStr[tokenLen++]=c;
					 c=ReadAChar(sFile);
					 tokenStr[tokenLen++]=c;
					 if(tokenStr[2]=='\'' && tokenStr[1] >= 32 && tokenStr[1] <= 127){
						token.token=SYN_CHAR;
						token.tokenVal.str=&tokenStr[1];
					 }
					 break;
			//end
			default: break;
		}
		break;
	}
	return(token);
}

void renewLex()
{
	prebuf=0;
}

static char ReadAChar(FILE *sFile)
{
	char c;
	if (prebuf!=0)
	{
		c=prebuf;
		prebuf=0;
	}
	else if (!feof(sFile))
		c=fgetc(sFile);
	else
		c=0;
	return(c);
}

static int FoundRELOOP()
{
	if (tokenStr[0]=='<' && tokenStr[1]!='=') return(SYN_LT);
	else if (tokenStr[0]=='<' && tokenStr[1]=='=') { prebuf=0; return(SYN_LE); }
	else if (tokenStr[0]=='>' && tokenStr[1]!='=') return(SYN_GT);
	else if (tokenStr[0]=='>' && tokenStr[1]=='=') { prebuf=0; return(SYN_GE); }
	else if (tokenStr[0]=='=' && tokenStr[1]!='=') return(SYN_SET);
	else if (tokenStr[0]=='=' && tokenStr[1]=='=') { prebuf=0; return(SYN_EQ); }  	// BUG_1 此处原来prebuf没有清空
	else if (tokenStr[0]=='!' && tokenStr[1]!='=') return(SYN_NOT);
	else if (tokenStr[0]=='!' && tokenStr[1]=='=') { prebuf=0; return(SYN_NE); }	// BUG_1 此处原来prebuf没有清空
	else if (tokenStr[0]=='&' && tokenStr[1]=='&') { prebuf=0; return(SYN_AND);}	// BUG_1 此处原来prebuf没有清空
	else if (tokenStr[0]=='|' && tokenStr[1]=='|') { prebuf=0; return(SYN_OR); }	// BUG_1 此处原来prebuf没有清空
	else return(ERR);
}

static int STR2INT()
{
	int i,s=0;
	for (i=0;i<tokenLen;i++)
		s=s*10+tokenStr[i]-'0';
	return(s);
}

static int FoundKeyword()
{
	if (strcompare(tokenStr,"TRUE")) return(SYN_TRUE);
	if (strcompare(tokenStr,"FALSE")) return(SYN_FALSE);
	if (strcompare(tokenStr,"int")) return(SYN_INT);
	if (strcompare(tokenStr,"char")) return(SYN_CHAR);
	if (strcompare(tokenStr,"if")) return(SYN_IF);
	if (strcompare(tokenStr,"else")) return(SYN_ELSE);
	if (strcompare(tokenStr,"while")) return(SYN_WHILE);
	if (strcompare(tokenStr,"show")) return(SYN_SHOW);
	if (strcompare(tokenStr,"continue")) return(SYN_CTN);
	if (strcompare(tokenStr,"break")) return(SYN_BRK);
	return(SYN_ID);
}

static int strcompare(char *sstr, char*tstr)
{
	while (*sstr==*tstr && *sstr!='\0') { sstr++; tstr++; }
	if (*sstr=='\0' && *tstr=='\0')	return(1);
	else return(0);
}
