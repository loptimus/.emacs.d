/** -*- coding: latin-1 -*-
 * 
 * The  contents of this  file are  subject to  the Erlang  Public License,
 * Version  1.1, (the  "License");  you may  not  use this  file except  in
 * compliance  with the License.  You should  have received  a copy  of the
 * Erlang  Public License  along  with this  software.  If not,  it can  be
 * retrieved at http://plc.inf.elte.hu/erlang/
 *
 * Software  distributed under  the License  is distributed  on an  "AS IS"
 * basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
 * the License  for the specific language governing  rights and limitations
 * under the License.
 *
 * The Original Code is RefactorErl.
 *
 * The Initial Developer of the  Original Code is Eötvös Loránd University.
 * Portions created  by Eötvös Loránd University are  Copyright 2008-2009,
 * Eötvös Loránd University. All Rights Reserved.
 *
 * @author Peter Felker <felker.peter88@gmail.com>
 * 
 */

#ifndef __exception_helper_h_
#define __exception_helper_h_

#include <iostream>
#include <exception>

/** The purpose of this class is that we can easily define exceptions with
 *  the DEFINE_EXCEPTION(EXCEPTION_NAME, MESSAGE) macro. */
class exception_helper : public std::exception {
public:

    exception_helper(const char* msg);

    exception_helper(const std::string& msg);

    const char* what() const throw();

    virtual ~exception_helper() throw();

private:

    std::string message;

};

#endif
