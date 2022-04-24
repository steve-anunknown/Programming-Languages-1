#include <stdio.h>
#include <stdlib.h>
#define MAX 10000

int min_gas_towns[MAX] = { 0 } ;

int main (int argc, char * argv[])
{
	for (unsigned int i = 0 ; i < MAX ; ++i) min_gas_towns[i] = -1; //initialize array
	if (argc < 2) exit(1);
	
	FILE *infile;
	infile = fopen(argv[1],"r");
	
	unsigned int number = 0;
	unsigned int towns = 0;
	unsigned int roads = 0;
	
	fscanf(infile, "%u", &number); //read number of towns
	if(!feof(infile)) towns = number;
	else exit(1);
	fscanf(infile, "%u", &number);
	if(!feof(infile)) roads = number;
	else exit(1);
	
	unsigned int from = 0;
	unsigned int to = 0;
	unsigned int weight = 0;
	for (unsigned int i = 0 ; i < roads ; ++i) //for each town find the cheapest road
	{
		fscanf(infile, "%u", &number);
		from = number - 1;
		fscanf(infile, "%u", &number);
		to = number - 1;
		fscanf(infile, "%u", &number);
		weight = number;
		
		if (min_gas_towns[from] == -1 || weight < (unsigned)min_gas_towns[from]) min_gas_towns[from] = weight;
		if (min_gas_towns[to] == -1 || weight < (unsigned)min_gas_towns[to]) min_gas_towns[to] = weight;
	}
	fclose(infile);
	
	unsigned int minimum_gas = 0;
	for (unsigned int i = 0 ; i < towns ; ++i)
	{
		if (minimum_gas < (unsigned)min_gas_towns[i]) minimum_gas = min_gas_towns[i];
	}
	printf("%d\n", minimum_gas);
	return 0;
}