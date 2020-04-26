m_local__x_3 = None
m_local__xs_4 = None
def map():
	return (lambda f_local__f_1:lambda f_local__l_2:(({'Nil':None,}) if (match(f_local__l_2,{'Nil':None,})) else ((({'Cons':((f_local__f_1) (m_local__x_3),((map()) (f_local__f_1)) (m_local__xs_4)),}) if (match(f_local__l_2,{'Cons':("m_local__x_3","m_local__xs_4"),})) else (exit())))))
def map_id():
	return (((map()) (lambda f_local__x_5:f_local__x_5)) ({'Cons':(1,{'Cons':(2,{'Cons':(3,{'Nil':None,}),}),}),}))
m_local__x_8 = None
m_local__xs_9 = None
def head():
	return (lambda f_local__l_6:lambda f_local__def_7:((f_local__def_7) if (match(f_local__l_6,{'Nil':None,})) else (((m_local__x_8) if (match(f_local__l_6,{'Cons':("m_local__x_8","m_local__xs_9"),})) else (exit())))))

def match(pat1, pat2):
	print(pat1,pat2)
	print(type(pat1),type(pat2))
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
	print(((head()) (map_id())) (5))

