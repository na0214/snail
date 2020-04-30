def _local__g_1(_ctx):
    return ((_local__e_4)({"_local__e_4": _local__e_4,}))(3)


def _local__e_4(_ctx):
    return lambda f_local__x_5: f_local__x_5


def _local__z_2(_ctx):
    return lambda f_local__x_3: f_local__x_3


def f():
    return ((_local__z_2)({"_local__z_2": _local__z_2, "_local__g_1": _local__g_1,}))(
        (_local__g_1)({"_local__z_2": _local__z_2, "_local__g_1": _local__g_1,})
    )


def main():
    return f()


def match(pat1, pat2):
    if (type(pat2) is dict) and (type(pat1) is dict):
        if list(pat2.keys())[0] == list(pat1.keys())[0]:
            return match(pat2[list(pat2.keys())[0]], pat1[list(pat1.keys())[0]])
        else:
            return False
    elif (type(pat2) is tuple) and (type(pat1) is tuple):
        _f = True
        for _i, _t in enumerate(pat2):
            _f = match(pat1[_i], _t) and _f
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
