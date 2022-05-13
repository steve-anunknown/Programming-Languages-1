#include <iostream>
#include <fstream>
#include <algorithm>
#define MAXV 10000
#define MAXE 100000

class edge
{
	public:
		unsigned int start;
		unsigned int end;
		unsigned int weight;
		edge():start(0),end(0),weight(0){};
		friend bool operator < (const edge &edge1, const edge &edge2)
		{
			return edge1.weight < edge2.weight;
		}
};
class disjointSet
{
	public:
		unsigned int size;
		unsigned int parent[MAXV];
		unsigned int rank[MAXV];
		disjointSet(const unsigned int &size)
		{
			for (unsigned int i = 0; i < size; ++i) 
			{
				parent[i] = i;
				rank[i] = 0;
			}
		}
		/*
		 * source for find function
		 * https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Finding_set_representatives
		 */ 
		unsigned int find(const unsigned int &num)
		{
			if (parent[num] == num) return num;
			else return find(parent[num]);
		}
		/* 
		 * source for the merge function
		 * https://www.geeksforgeeks.org/kruskals-minimum-spanning-tree-algorithm-greedy-algo-2/
		 */
		void merge(unsigned int num1, unsigned int num2)
		{
			unsigned int par1 = find(num1);
			unsigned int par2 = find(num2);
			if (par1!=par2)
			{
				if (rank[par1] < rank[par2])
				{
					parent[par1] = par2;
					rank[par2] += rank[par1];
				}
				else
				{
					parent[par2] = par1;
					rank[par1] += rank[par2];
				}
			}
		}

};


unsigned int Kruskal(edge map[], disjointSet &set, const unsigned int &numroads)
{
	unsigned int maximum = 0;
	std::sort(map,map+numroads); /* sort the edges in order to perform the algorithm */
	for (unsigned int i = 0; i < numroads; ++i)
	{
		/*
		 * if the edges have different parents, then a
		 * circle is not formed and, as a result, the
		 * edge belongs to the minimum spanning tree
		 */
		if (set.find(map[i].start)!=set.find(map[i].end))
		{
			set.merge(set.find(map[i].start),set.find(map[i].end));
			if (maximum == 0 || map[i].weight >= maximum) maximum= map[i].weight; /* keep the heaviest edge of the tree */
		}

	}
	return maximum;
}

edge graph[MAXE];
disjointSet disSet(MAXV);

int main (int argc, char *argv[])
{
	unsigned int towns = 0;
	unsigned int roads = 0;
	std::fstream inputfile(argv[1], std::ios_base::in);
	inputfile >> towns >> roads;
	unsigned int start = 0;
	unsigned int end = 0;
	for (unsigned int i = 0; i < roads; ++i)
	{
		inputfile >> start >> end >> graph[i].weight;
		graph[i].start = start-1;
		graph[i].end = end-1;
	}	
	std::cout << Kruskal(graph,disSet,roads) << std::endl;
	return 0;
}
