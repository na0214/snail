m_local__x_2 = None
m_local__xs_3 = None
def a():
	return (lambda f_local__l_1:(("even") if (match(f_local__l_1,{'Nil':None,})) else ((((b()) (m_local__xs_3)) if (match(f_local__l_1,{'Cons':("m_local__x_2","m_local__xs_3"),})) else (exit())))))
m_local__x_2 = None
m_local__xs_3 = None
def b():
	return (lambda f_local__l_1:(("odd") if (match(f_local__l_1,{'Nil':None,})) else ((((a()) (m_local__xs_3)) if (match(f_local__l_1,{'Cons':("m_local__x_2","m_local__xs_3"),})) else (exit())))))
def _local__p_4(_ctx):
	return (lambda f_local__x_6:{'Cons':(1,((_ctx["_local__q_4"]) ({'f_local__x_6':f_local__x_6,'_local__p_4':_ctx["_local__p_4"],'_local__q_4':_ctx["_local__q_4"],})) (f_local__x_6)),})
def _local__q_4(_ctx):
	return (lambda f_local__x_5:{'Cons':(2,((_ctx["_local__p_4"]) ({'f_local__x_5':f_local__x_5,'_local__p_4':_ctx["_local__p_4"],'_local__q_4':_ctx["_local__q_4"],})) (f_local__x_5)),})
def f():
	return ((_local__p_4) ({'_local__p_4':_local__p_4,'_local__q_4':_local__q_4,}))
def main():
	return ((a()) ({'Cons':(1,{'Cons':(2,{'Cons':(3,{'Cons':(4,{'Nil':None,}),}),}),}),}))

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

