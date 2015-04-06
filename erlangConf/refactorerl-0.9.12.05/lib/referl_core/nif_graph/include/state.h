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

#ifndef __state_h_
#define	__state_h_


#include <iostream>
#include <fstream>

#include "algorithms.h"
#include "types.h"
#include "exception_helper.h"

/** The base of the backup class. */
class state {
public:

    /** Constructor.
     *
     *  @param name The state's name. Default: (empty string)
     */
    state(const std::string& name = "");


    //-------------------------------------------------------------------------
    // File read/write methods

    /** Writes out the name of the state to the given file,
     *  if the name is not empty.
     *
     *  @param f The text file into which the function writes.
     */
    void write_name(std::fstream& f) const;

    /**  Reads the state's name from the given file.
     *
     *   @param f The text file from which the function reads.
     */
    void read_name(std::fstream& f);


    //-------------------------------------------------------------------------
    // Getters and setters

    /** Returns the path of the state. */
    std::string get_path() const;

    /** Sets the name of the state, which is the backup's file name also.
     *
     *  @param name The new name of the backup.
     */
    virtual void set_name(const std::string& name);

    /** Returns the name of the state. */
    std::string get_name() const;

    virtual ~state() {}

protected:

    //-------------------------------------------------------------------------
    // Properties

    /** The name of the state, that is the file's name also. */
    std::string name;

};


#endif
