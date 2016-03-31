from __future__ import print_function


class Printer(object):
    def load(self):
        print("load")
        self.a = 'a'
    def run(self, *args, **kwargs):
        print("self.a = {}".format(self.a))
        print("*args = {}".format(args))
        print("*kwargs = {}".format(kwargs))

