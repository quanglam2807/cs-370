/**
 * <fileName>
 * Author: Kent D. Lee (c) 2017 Created on Jan 1, 2017.
 *
 * License: Please read the LICENSE file in this distribution for details
 * regarding the licensing of this code. This code is freely available for
 * educational use. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 * KIND.
 *
 * Description:
 */
package jcoco;

import java.util.ArrayList;

public class PyExceptionType extends PyType {

    public PyExceptionType() {
        super("Exception", PyType.PyTypeId.PyExceptionTypeId);
    }

    @Override
    public PyObject __call__(ArrayList<PyObject> args) {
        if (args.size() != 1) {
            throw new PyException(PyException.ExceptionType.PYWRONGARGCOUNTEXCEPTION, "TypeError: expected 1 arguments, got " + args.size());
        }
        
        String s = ((PyStr)args.get(0)).str();
        return new PyException(PyException.ExceptionType.PYEXCEPTION, s);
    }
}
