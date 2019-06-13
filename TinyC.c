#include "stdio.h"

extern void SyntaxAnalysis();

FILE *sFile;

int main(int argc, char* argv[])
{
    //sFile=fopen_s(argv[1],"rt");
    sFile=fopen("test1.txt","rt");
	SyntaxAnalysis();
	fclose(sFile);

//Free all memories

	return 0;
}
