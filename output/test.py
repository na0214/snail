def f():
	return (lambda f_local__x_1:f_local__x_1)
def g():
	return (lambda f_local__y_2:lambda f_local__x_3:f_local__y_2)
def some_2():
	return ({'Some':2,})
def some_str():
	return ({'Some':"abc",})
def none():
	return ({'None':None,})
def a():
	return (lambda f_local__b_4:lambda f_local__c_5:f_local__b_4)
def _local__f_6(_ctx):
	return (lambda f_local__x_7:f_local__x_7)
def b():
	return (((_local__f_6) ({'_local__f_6':_local__f_6,})) (2))
def fn():
	return (lambda f_local__x_8:f_local__x_8)
m_local__a_10 = None
m_local__b_11 = None
def tuple():
	return (lambda f_local__x_9:((m_local__b_11) if (match(f_local__x_9,("m_local__a_10","m_local__b_11"))) else (exit())))
m_local__x_14 = None
m_local__xs_15 = None
def map():
	return (lambda f_local__f_12:lambda f_local__l_13:(({'Nil':None,}) if (match(f_local__l_13,{'Nil':None,})) else ((({'Cons':((f_local__f_12) (m_local__x_14),((map()) (f_local__f_12)) (m_local__xs_15)),}) if (match(f_local__l_13,{'Cons':("m_local__x_14","m_local__xs_15"),})) else (exit())))))
m_local__a_17 = None
def get_left_int():
	return (lambda f_local__e_16:((m_local__a_17) if (match(f_local__e_16,{'Left':"m_local__a_17",})) else (exit())))
m_local__x_19 = None
m_local__xs_20 = None
m_local__xxs_21 = None
def fst_A_int():
	return (lambda f_local__a_18:((m_local__x_19) if (match(f_local__a_18,{'A':(("m_local__x_19","m_local__xs_20"),"m_local__xxs_21"),})) else (exit())))
def main():
	return (((g()) (2)) (3))

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

