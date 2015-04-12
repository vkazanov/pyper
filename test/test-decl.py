# Used in ArenaMgr queue info.
VEHICLE_CLASSES = \
(
	'lightTank',
	'mediumTank',
	'heavyTank',
	'SPG',
	'AT-SPG',
)

var1 = 1

var2, var3 = 1, 2

var, = 1,

var5, var6, var7 = 1, 2, 3

class Test:
	var4 = 2

	var5 = 4

def fun():
	pass

fun()

def outerFun():
	def innerFun(fun):
		def innerInner():
			pass

try:
	pass
except:
	pass
else:
	pass
finally:
	pass

def __div__(self, other):  # Python 2 compatibility
	return type(self).__truediv__(self, other)

if __name__ == '__main__':
    pass
