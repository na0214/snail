def tuple_1():
	return (((1,2),3))
def tuple_2():
	return (((1,2),3))
m_local__a_2 = None
m_local__b_3 = None
def fst():
	return (lambda f_local__tuple_1:((m_local__a_2) if (match(f_local__tuple_1,("m_local__a_2","m_local__b_3"))) else (exit())))
def main():
	return (((fst()) (tuple_1()),(fst()) (tuple_2())))

def match(pat1, pat2):
	if (type(pat2) is dict) and (type(pat1) is dict):
		if list(pat2.keys())[0] == list(pat1.keys())[0]:
			return match(pat2[list(pat2.keys())[0]], pat1[list(pat1.keys())[0]])
		else:
			return False
	elif (type(pat2) is tuple) and (type(pat1) is tuple):
		_f = True
		for _i, _t in enumerate(pat2):
			_f = match(pat1[_i],_t) and _f
		return _f
	elif (type(pat1) is str) and (pat1[0] == "m"):
		globals()[pat1] = pat2
		return True
	elif (type(pat2) is str) and (pat2[0] == "m"):
		globals()[pat2] = pat1
		return True
	elif pat1 == None and pat2 == None:
		return True
	else:
		return False

if __name__ == "__main__":
	print(main())

