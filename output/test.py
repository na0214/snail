def v():
	return ({'Cons':(1,{'Nil':None,}),})
m_local__x_1 = None
m_local__xs_2 = None

def match(pat1, pat2):
	if (type(pat2) is dict) and (type(pat1) is dict):
		if list(pat2.keys())[0] == list(pat1.keys())[0]:
			return match(pat2[list(pat2.keys())[0]], pat1[list(pat1.keys())[0]])
		else:
			return False
	elif (type(pat2) is tuple) and (type(pat1) is tuple):
		_f = True
		for _i, _t in enumerate(pat2):
			_f = match(_t,pat1[_i]) and _f
		return _f
	elif (type(pat1) is str) and (pat1[0] == "m"):
		globals()[pat1] = pat2
		return True
	elif (type(pat2) is str) and (pat2[0] == "m"):
		globals()[pat2] = pat1
		return True
	else:
		return False
if __name__ == "__main__":
	print(((2) if (match(v(),{'Nil':None,})) else (((m_local__x_1) if (match(v(),{'Cons':("m_local__x_1","m_local__xs_2"),})) else (exit())))))

