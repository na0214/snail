def print_string():
	return (lambda x:print(x,end=""))
def print_int():
	return (lambda x:print(x,end=""))
def print_float():
	return (lambda x:print(x,end=""))
def _h():

	return (lambda x:lambda y:{'True':None} if x < y else {'False':None})
def _i():

	return (lambda x:lambda y:{'True':None} if x > y else {'False':None})
def _l_l():

	return (lambda x:lambda y:{'True':None} if x == y else {'False':None})
def _h_l():

	return (lambda x:lambda y:{'True':None} if x <= y else {'False':None})
def _i_l():

	return (lambda x:lambda y:{'True':None} if x >= y else {'False':None})
def main():
	return ((((print_string()) ("true\n")) if (match(((_i_l()) (1)) (2),{'True':None,})) else ((((print_string()) ("false\n")) if (match(((_i_l()) (1)) (2),{'False':None,})) else (exit())))))

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
	main()

