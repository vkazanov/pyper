import datetime

from django.db.models.aggregates import refs_aggregate
from django.db.models.constants import LOOKUP_SEP
from django.utils import tree

class ExpressionNode(tree.Node):

    def __div__(self, other):  # Python 2 compatibility
        return type(self).__truediv__(self, other)
