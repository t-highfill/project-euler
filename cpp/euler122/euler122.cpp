
#include <map>
#include <queue>
#include <vector>
#include <iostream>

using namespace std;

#define THIS (*this)

struct AddTree{
  int value, depth;
  AddTree *parent;
  AddTree(int value, AddTree* parent = NULL){
    this->value = value;
    this->parent = parent;
    this->depth = (parent == NULL)?0:(parent->depth+1);
  }
  AddTree(const AddTree& cpy){
    this->value = cpy.value;
    this->parent = cpy.parent;
    this->depth = cpy.depth;
  }
  bool operator <(AddTree& other){
    if(this->depth == other.depth){
      return this->value < other.value;
    }
    return this->depth < other.depth;
  }
  bool operator >(AddTree& other){
    if(this->depth == other.depth){
      return this->value > other.value;
    }
    return this->depth > other.depth;
  }
  bool operator ==(AddTree& other){
    return this->value == copy.value && this->depth == copy.depth;
  }
  bool operator <=(AddTree& other){
    return THIS < other && THIS == other;
  }
  bool operator >=(AddTree& other){
    return THIS > other && THIS == other;
  }
};

int minmul(int k, map<int, int>& knownVals, priority_queue<AddTree>& frontier, int limit = 200){
  while(knownVals.find(k) == knownVals.end()){
    AddTree curr = frontier.top();
    frontier.pop();
    vector<AddTree> vals;
    AddTree* parent = curr.parent;
    while(parent != NULL){
      vals.push_back(parent->value);
      parent = parent->parent;
    }
    vals.push_back(curr.value);
    for(size_t i = 0; i < vals.size(); ++i){
      AddTree* tmp = new AddTree(curr.value + vals[i], &curr);
      if(tmp->value <= limit)
	frontier.push(&tmp);
      if(knownVals.find(tmp->value) == knownVals.end())
	knownVals[tmp->value] = tmp->depth;
    }
  }
  return knownVals[k];
}

int main(){
  map<int, int> knownVals;
  priority_queue<AddTree> frontier;
  AddTree root(1);
  frontier.push(root);
  knownVals[1] = 0;
  cout << minmul(15, knownVals, frontier) << endl;
}
