class OldStyleEmpty:
	pass

class OldStyleEmpty2: pass

class OldStyleSingleMethod:
	def test(self):
		pass

class OldStyleMethods:
	def test(self):
		pass

	def test2(self):
		pass

###

class NewStyleEmpty(object):
	pass

###

def functionSimple(arg1, arg2):
	pass

def functionSimple2(arg1, arg2): pass

def functionLong(arg1, arg2):
	something = 1

	print something
	# test
