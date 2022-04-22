// Caesar Cipher

#include <stdio.h>
#include <cstdio>
#include <math.h>
#include <string.h>
#define MAX 10000
#define SIZE 26

//global variables

double freq_english[SIZE] =
{
	0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
	0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
	0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
	0.00978, 0.02360, 0.00150, 0.01974, 0.00074
};
					  
char buffer[MAX] = "";
char candidate[MAX] = "";
char result[MAX] = "";

//end of global variables

// helpful functions

void copy(char original[], char final[])
{
	for (unsigned int i=0; i<strlen(original); ++i)
		final[i] = original[i];
}

unsigned int ischar(unsigned int arg)
{
	if ('A'<=arg && arg<='Z') return 1; //upper case
	else if ('a'<=arg && arg<='z') return 2; //lower case
	else return 0; //not char
}

double entropy(double freq_decr[], double freq_normal[])
{
	double result_entropy = 0;
	for (unsigned int i=0; i < SIZE; ++i)
		if (freq_normal[i]!=0)	result_entropy+=freq_decr[i]*log(freq_normal[i]);
	return -result_entropy;
}

void calc_freqs(const char text[], double freqs[])
{
	double total = 0;
	unsigned int flag = 0;
	unsigned int counts[SIZE] = { 0 };
	//printf("%s ",text);
	//printf("%u ",(unsigned int)strlen(text));
	for (unsigned int i=0; i<strlen(text); ++i)
	{
		flag = ischar(text[i]);
		if (flag==1) //if char is upper case
		{
			++total;
			++counts[text[i]-'A'];
		}
		else if (flag==2) //if char is lower case
		{
			++total;
			++counts[text[i]-'a'];
		}
	}
	for (unsigned int i=0; i<SIZE; ++i) 
	{
		freqs[i]=counts[i]/total;
		//if (counts[i]!=0) printf("count %u ",counts[i]);
	}
	//printf("total %u\n",total);
}

void decypher(const char encrypted[], char temp[], char res[])
{
	double freqs[SIZE] = { 0 };
	double min = MAX;
	for (unsigned int n=0; n<SIZE; ++n)
	{
		for (unsigned int i=0; i< strlen(encrypted); ++i)
		{
			unsigned int flag = ischar(encrypted[i]);
			if (flag==1)
			{
				temp[i] = encrypted[i]- n;
				if (temp[i]<'A') temp[i] = temp[i] +'Z'-'A'+1;
			}
			else if (flag==2)
			{
				temp[i] = encrypted[i]- n;
				if (temp[i]<'a') temp[i] = temp[i]+'z'-'a'+1;
			}
			else temp[i] = encrypted[i];
		}
		//printf("%s \n",temp);
		calc_freqs(temp,freqs);
		double entr=entropy(freqs,freq_english);
		//printf("%f \n", entr);
		if (entr<=min) 
		{
			min = entr;
			copy(temp,res); //copy temp to result
		}
	}
}

//end of helpful functions

int main(int argc, char *argv[])
{
	if (argc != 2) exit(1);
	FILE *input;
	input=fopen(argv[1],"r");
	char c;
	unsigned int counter = 0;
	while ((c=fgetc(input))!=EOF)
		buffer[counter++]=c;
	printf("%s \n",buffer);
	fclose(input);
	decypher(buffer, candidate, result);
	printf("%s \n",result);
	return 0;
}