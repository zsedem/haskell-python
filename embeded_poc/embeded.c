#include <Python.h>
static int listmutator=0;

/* Return the number of arguments of the application command line */
int
main(int argc, char *argv[]){
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
    }
    PyMethodDef EmbMethods[] = {
        {"listmutator", listmutator, METH_VARARGS,
         "Return the number of arguments received by the process."},
        {NULL, NULL, 0, NULL}
    };
    PyModuleDef EmbModule = {
        PyModuleDef_HEAD_INIT, "emb", NULL, -1, EmbMethods,
        NULL, NULL, NULL, NULL
    };


    PyObject* PyInit_emb(void)
    {
        return PyModule_Create(&EmbModule);
    }
    PyImport_AppendInittab("emb", &PyInit_emb);
    wchar_t *program = Py_DecodeLocale(argv[0], NULL);
    if (program == NULL) {
        fprintf(stderr, "Fatal error: cannot decode argv[0]\n");
        exit(1);
    }
    Py_SetProgramName(program);  /* optional but recommended */
    Py_Initialize();
    PyRun_SimpleString("from emb import listmutator\n"
                       "a = [4, 5, 6, 7]\n"
                       "listmutator(a)\n"
                       "import pdb; pdb.set_trace()"
                       );
    Py_Finalize();
    PyMem_RawFree(program);
    return 0;
}
