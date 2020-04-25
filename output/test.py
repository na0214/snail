Nil = None
def _local__a_1(_ctx):
	return (3)
def _local__b_2(_ctx):
	return ((_ctx["_local__a_1"]) ({'_local__a_1':_ctx["_local__a_1"],}))
def _local__c_3(_ctx):
	return ((_ctx["_local__b_2"]) ({'_local__b_2':_ctx["_local__b_2"],'_local__a_1':_ctx["_local__a_1"],}))
def f():
	return ((_local__c_3) ({'_local__c_3':_local__c_3,'_local__b_2':_local__b_2,'_local__a_1':_local__a_1,}))
def _local__gf_5(_ctx):
	return (lambda f_local__x_6:f_local__x_6)
def g():
	return (lambda f_local__z_4:((_local__gf_5) ({'_local__gf_5':_local__gf_5,'f_local__z_4':f_local__z_4,})) (f_local__z_4))
def p():
	return ((g()) (2))
if __name__ == "__main__":
	p()

