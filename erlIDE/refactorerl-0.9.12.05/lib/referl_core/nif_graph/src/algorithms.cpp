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
#include <algorithm>
#include <list>
#include <vector>
#include <sstream>
#include <string>

#include "algorithms.h"

using namespace std;

strings_t str_split(const string& str, const string& delimiter) {
    strings_t tokens;
    if(str.size() > 0) {
        string::size_type lastPos = str.find_first_not_of(delimiter, 0);
        string::size_type pos     = str.find_first_of(delimiter, lastPos);

        while (string::npos != pos || string::npos != lastPos) {
            tokens.push_back(str.substr(lastPos, pos - lastPos));
            lastPos = str.find_first_not_of(delimiter, pos);
            pos = str.find_first_of(delimiter, lastPos);
        }
    }

    return tokens;
}

bool str_starts_with(const string& str, const string& prefix) {
    if(str.size() >= prefix.size()) {
        return (str.compare(0, prefix.size(), prefix) == 0);
    } else {
        return false;
    }
}

bool str_contains(const string& str, const char* infix) {
    return str.find(infix, 0) != string::npos;
}

bool str_contains(const string& str, const string& infix) {
    return str_contains(str, infix.c_str());
}

bool str_ends_with(const string& str, const string& suffix) {
    if(str.size() >= suffix.size()) {
        return (str.compare(str.size() - suffix.size(), str.size(), suffix) == 0);
    } else {
        return false;
    }
}
