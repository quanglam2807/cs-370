/**
 * PyFrozenSet.java
 * Author: Quang Lam (c) 2018 Created on March 12 2019.
 */

package jcoco;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import jcoco.PyType.PyTypeId;

public class PyFrozenSet extends PyPrimitiveTypeAdapter {

    private HashSet<PyObject> data;

    public PyFrozenSet(HashSet<PyObject> data) {
        super("frozenset", PyTypeId.PyFrozenSetType);
        this.data = data;
        initMethods(funs());
    }
    
    public int len() {
        return this.data.size();
    }

    public HashSet<PyObject> hashSet() {
        return this.data;
    }

    @Override
    public String str() {
        String str = "frozenset({";
        ArrayList<PyObject> args = new ArrayList<PyObject>();

        int i = 0;
        for (PyObject obj:data) {
            str += ((PyStr) obj.callMethod("__repr__", args)).str();

            if (i < data.size() - 1) {
                str += ", ";
            }

            i = i + 1;
        }

        str += "})";

        return str;
    }

    public static HashMap<String, PyCallable> funs() {
        HashMap<String, PyCallable> funs = new HashMap<String, PyCallable>();

        funs.put("__len__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 1)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 1 arguments, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(0);
                return new PyInt(self.data.size());
            }
        });

        funs.put("__iter__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 1) {
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 1 arguments, got " + args.size());
                }

                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);
                return new PyFrozenSetIterator(self);
            }
        });

        funs.put("__notin__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 2 arguments, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(1);
                PyObject element = args.get(0);
                return new PyBool(!self.data.contains(element));
            }
        });

        funs.put("__contains__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 2 arguments, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(1);
                PyObject element = args.get(0);
                return new PyBool(self.data.contains(element));
            }
        });

        funs.put("__or__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() < 1)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at least 1 argument, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);
                HashSet<PyObject> result = (HashSet<PyObject>) self.hashSet().clone();
                for (int i = 0; i < args.size() - 1; i++){
                    result.addAll(((PyFrozenSet) args.get(i)).data);
                }
                return new PyFrozenSet(result);
            }
        });

        funs.put("__lt__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 2 arguments, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);
                PyFrozenSet other = (PyFrozenSet) args.get(0);

                return new PyBool(other.data.containsAll(self.data));
            }
        });

        funs.put("__ge__", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 2 arguments, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);
                PyFrozenSet other = (PyFrozenSet) args.get(0);

                return new PyBool(self.data.containsAll(other.data));
            }
        });

        funs.put("union", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() < 1)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at least 1 argument, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(args.size() - 1);
                HashSet<PyObject> result = (HashSet<PyObject>) self.hashSet().clone();
                for (int i = 0; i < args.size() - 1; i++){
                    result.addAll(((PyFrozenSet) args.get(i)).data);
                }
                return new PyFrozenSet(result);
            }
        });

        funs.put("intersection", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() < 1)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at least 1 argument, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(args.size() - 1);
                HashSet<PyObject> result = (HashSet<PyObject>) self.hashSet().clone();
                for (int i = 0; i < args.size() - 1; i++){
                    result.retainAll(((PyFrozenSet) args.get(i)).data);
                }
                return new PyFrozenSet(result);
            }
        });

        funs.put("copy", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 1)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected 1 argument, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(args.size() - 1);
                HashSet<PyObject> result = (HashSet<PyObject>) self.hashSet().clone();
                return new PyFrozenSet(result);
            }
        });

        funs.put("difference", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at 2 arguments, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(args.size() - 1);
                HashSet<PyObject> result = (HashSet<PyObject>) self.hashSet().clone();
                for (int i = 0; i < args.size() - 1; i++){
                    result.removeAll(((PyFrozenSet) args.get(i)).data);
                }
                return new PyFrozenSet(result);
            }
        });

        funs.put("symmetric_difference", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at 2 arguments, got " + args.size());
                PyFrozenSet self= (PyFrozenSet) args.get(args.size() - 1);

                // union - intersection
                PyFrozenSet union = (PyFrozenSet) self.callMethod("union", selflessArgs(args));
                PyFrozenSet intersection = (PyFrozenSet) self.callMethod("intersection", selflessArgs(args));
                ArrayList<PyObject> resultArgs = new ArrayList<PyObject>();
                resultArgs.add(intersection);
                PyFrozenSet result = (PyFrozenSet) union.callMethod("difference", resultArgs);
                return result;
            }
        });

        funs.put("issuperset", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at 2 arguments, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);
                PyFrozenSet other = (PyFrozenSet) args.get(0);

                return new PyBool(self.data.containsAll(other.data));
            }
        });

        funs.put("isdisjoint", new PyCallableAdapter() {
            @Override
            public PyObject __call__(ArrayList<PyObject> args) {
                if (args.size() != 2)
                    throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION,
                            "TypeError: expected at 2 arguments, got " + args.size());
                PyFrozenSet self = (PyFrozenSet) args.get(args.size() - 1);

                PyFrozenSet union = (PyFrozenSet) self.callMethod("union", selflessArgs(args));

                return new PyBool(union.data.isEmpty());
            }
        });

        return funs;
    }
}
