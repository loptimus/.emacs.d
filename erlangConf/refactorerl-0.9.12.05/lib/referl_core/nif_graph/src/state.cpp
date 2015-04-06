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

#include <iostream>
#include <fstream>
#include <cctype>
#include <exception>

#include "algorithms.h"
#include "conversions.h"
#include "backup.h"
#include "directory.h"
#include "globals.h"

using namespace std;


state::state(const string& name) {
    set_name(name);
}


//-----------------------------------------------------------------------------
// File read/write methods

void state::write_name(fstream& f) const {
    f << name;
}

void state::read_name(fstream& f) {
    string name;

    f >> name;
    set_name(name);
}


//-----------------------------------------------------------------------------
// Getters and setters

string state::get_path() const {
    return globals::ngraph->get_path() + name;
}

void state::set_name(const string& name) {
    this->name = name;
}

string state::get_name() const {
    return name;
}
