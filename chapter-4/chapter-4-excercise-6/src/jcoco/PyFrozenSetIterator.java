/**
 * PyFrozenSetInterator.java
 * Author: Quang Lam (c) 2018 Created on March 12 2019.
 */
package jcoco;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class PyFrozenSetIterator extends PyPrimitiveTypeAdapter {

    private PyFrozenSet frozenSet;
    private Iterator<PyObject> hashSetIterator;

    public PyFrozenSetIterator(PyFrozenSet frozenSet) {
        super("frozenset_iterator", PyType.PyTypeId.PyFrozenSetIteratorType);
        this.frozenSet = frozenSet;
        initMethods(funs());
        this.hashSetIterator = this.frozenSet.hashSet().iterator();
    }

    public static HashMap<String, PyCallable> funs() {
        HashMap<String, PyCallable> funs = new HashMap<String, PyCallable>();

        funs.put("__iter__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 1) {
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION, "TypeError: expected 1 arguments, got " + args.size() + ".");
                }

                PyFrozenSetIterator self = (PyFrozenSetIterator) args.get(args.size() - 1);

                return self;
            }
        });

        funs.put("__next__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 1) {
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION, "TypeError: expected 1 arguments, got " + args.size() + ".");
                }

                PyFrozenSetIterator self = (PyFrozenSetIterator) args.get(args.size() - 1);

                if (self.hashSetIterator.hasNext()) {
                    return self.hashSetIterator.next();
                }

                throw new PyException(PyException.ExceptionType.PYSTOPITERATIONEXCEPTION, "stop it");
            }
        });

        return funs;
    }
}
