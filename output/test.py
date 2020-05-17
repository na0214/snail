
def print_string():
	return (lambda x:print(x,end=""))
def i():
	return (3)
def f():
	return (2.3)
def s():
	return ("abc")
def map():
	return (lambda f_local__f_1:lambda f_local__l_2:(({'Nil':None,}) if (match(f_local__l_2,{'Nil':None,})) else ((({'Cons':((f_local__f_1) (m_local__x_3),((map()) (f_local__f_1)) (m_local__xs_4)),}) if (match(f_local__l_2,{'Cons':("m_local__x_3","m_local__xs_4"),})) else (exit())))))
def head():
	return (lambda f_local__l_5:((m_local__x_6) if (match(f_local__l_5,{'Cons':("m_local__x_6","m_local__xs_7"),})) else (exit())))
def head_int_list():
	return (lambda f_local__l_8:((m_local__x_9) if (match(f_local__l_8,{'Cons':("m_local__x_9","m_local__xs_10"),})) else (exit())))
def nil():
	return ({'Nil':None,})
def left_int():
	return ({'Left':1,})
def _local__i_11(_ctx):
	return (1)
def _local__f_12(_ctx):
	return (lambda f_local__x_13:f_local__x_13)
def local_let_annot():
	return (((_local__f_12) ({'_local__f_12':_local__f_12,'_local__i_11':_local__i_11,})) ((_local__i_11) ({'_local__f_12':_local__f_12,'_local__i_11':_local__i_11,})))
def _o_i():
	return (lambda f_local__x_14:lambda f_local__y_15:(f_local__y_15) (f_local__x_14))
def pipe_example():
	return (((_o_i()) (((_o_i()) (((_o_i()) ({'Cons':(1,{'Cons':(2,{'Nil':None,}),}),})) ((map()) (lambda f_local__x_17:f_local__x_17)))) (head()))) (lambda f_local__x_16:((_o_i()) ({'Cons':(f_local__x_16,{'Nil':None,}),})) (head_int_list())))
def main():
	return (pipe_example())

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

