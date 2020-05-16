
def print_string():
	return (lambda x:print(x,end=""))
def add():
	return (lambda f_local__x_1:lambda f_local__y_2:(f_local__x_1) + (f_local__y_2))
def sub():
	return (lambda f_local__x_3:lambda f_local__y_4:(f_local__x_3) - (f_local__y_4))
def mul():
	return (lambda f_local__x_5:lambda f_local__y_6:(f_local__x_5) * (f_local__y_6))
def div():
	return (lambda f_local__x_7:lambda f_local__y_8:(f_local__x_7) / (f_local__y_8))
def add23():
	return (((add()) (2)) (3))
def main():
	return ((print_string()) ("abc\n"))

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

