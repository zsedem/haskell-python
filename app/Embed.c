
#include <Python.h>

PyObject*
listmutator(PyObject *self, PyObject *args)
{
    PyObject *the_list;
    if(!PyArg_ParseTuple(args, "O|O:listmutator", &the_list))
        return NULL;
    if(!PyList_Check(the_list))
        return NULL;
    PyObject_Print(the_list, stdout, 0);
    Py_RETURN_NONE;
};

static PyMethodDef EmbMethods[] = {
    {"listmutator", listmutator, METH_VARARGS,
     "Return the number of arguments received by the process."},
    {NULL, NULL, 0, NULL}
};
static PyModuleDef EmbModule = {
    PyModuleDef_HEAD_INIT, "emb", NULL, -1, EmbMethods,
    NULL, NULL, NULL, NULL
};

PyObject* PyInit_emb(void)
{
    return PyModule_Create(&EmbModule);
};


PyObject* embeded_module_init(void* method) {

    EmbMethods[0].ml_meth = method;
    PyImport_AppendInittab("emb", &PyInit_emb);
}

