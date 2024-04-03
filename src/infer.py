from sys import argv

import numpy as np

def parse_vec(s):
	s = s[1:-1].strip(" []").split(" ")
	return np.array([float(x) for x in s if not x.isspace() and x != ""])

def parse_mat(s):
	# Remove the first and last brackets
	rows = s[1:-1].strip().split("]")
	rows = [parse_vec(r.strip() + "]") for r in rows if not r.isspace() and r != ""]
	return np.row_stack(rows)
	
IN_FEATS = 4
OUT_CATS = 3

import pickle
from csv import reader

def main():
	with open(argv[1], "rb") as f:
		model = pickle.load(f)
	
	a = float(input("Enter sepal length: "))
	b = float(input("Enter sepal width: "))
	c = float(input("Enter petal length: "))
	d = float(input("Enter petal width: "))
	X = np.array([[a, b, c, d]])

	print(model.predict_proba(X))


	
if __name__ == "__main__":
	main()