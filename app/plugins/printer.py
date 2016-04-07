from __future__ import print_function

class Printer(object):
    def load(self):
        print("load")
        self.a = ['a']

    def run(self, emitter, **kwargs):
        print("self.a = {}".format(self.a))
        print("*args = {}".format(emitter))
        print("*kwargs = {}".format(kwargs))
        import emb
        print(emb.emit)
        emb.emit(emitter, {'a': 'b'})
        print("self.a = {}".format(self.a))
