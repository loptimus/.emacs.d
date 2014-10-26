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
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "bin_file.h"
#include "types.h"

using namespace std;
#include <time.h>

//-------------------------------------------------------------------------
// Constructors and open operations

bin_file::bin_file() : fstream() { }

bin_file::bin_file(const char* path, const ios_base::openmode& mode)
                  : fstream()
{
    this->open(path, mode);
}

bin_file::bin_file(const string& path, const ios_base::openmode& mode)
                  : fstream()
{
    this->open(path.c_str(), mode);
}

void bin_file::open(const char* path, const ios_base::openmode& mode) {
    this->path = string(path);
    fstream::open(path, mode | ios::binary);

    chmod(path, 0777);
}

void bin_file::open(const string& path, const ios_base::openmode& mode) {
    this->open(path.c_str(), mode);
}


//-----------------------------------------------------------------------------
// Static functions

string bin_file::last_modification(const char* path) {
    struct stat st_buff;
    string last_modification;

    stat(path, &st_buff);
    last_modification = to_string(ctime(&(st_buff.st_mtime)));

    return last_modification.substr(0, last_modification.size() - 1);
}

string bin_file::last_modification(string path) {
    return last_modification(path.c_str());
}


//-----------------------------------------------------------------------------
// Write operations

void bin_file::write(const string& str) {
    size_type str_size = str.size();

    check_writability();

    write(str_size);
    fstream::write(str.c_str(), str_size);
}


//-----------------------------------------------------------------------------
// Read operations

void bin_file::read(string& str) {
    size_type size;
    char* buffer;

    check_readability();

    fstream::read((char*)&size, sizeof(size));

    buffer = new char[size];
    fstream::read(buffer, size);
    str = string(buffer, size);

    delete[] buffer;
}


//-----------------------------------------------------------------------------
// Validation methods

void bin_file::write_magic_number() {
    write(get_size());
}

size_type bin_file::read_magic_number() {
    size_type size;
    pos_type act_pos = tellg();

    seekg(0, ios_base::end);
    seekg(tellg() - (pos_type)sizeof(size));
    read(size);
    seekg(act_pos);

    return size;
}

bool bin_file::is_corrupted() {
    size_type size = get_size();

    return read_magic_number() != size - sizeof(size);
}

void bin_file::validate() {
    if(!is_open()) {
        throw file_open_error("Error while opening the file: " + get_path());
    }

    if(is_corrupted()) {
        throw corrupted_file("Corrupted file: " + get_path());
    }
}


//-----------------------------------------------------------------------------
// Other functions

string bin_file::get_last_modification() const {
    return last_modification(get_path());
}

string bin_file::get_path() const {
    return path;
}

size_type bin_file::get_size() {
    size_type size;
    pos_type act_pos = tellg();

    seekg(0, ios_base::end);
    size = tellg();
    seekg(act_pos);

    return size;
}

bin_file::~bin_file() {
    close();
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------

void bin_file::check_readability() const {
    if(!good()) {
        throw file_read_error("File read error: " + get_path());
    }
}

void bin_file::check_writability() const {
    if(!good()) {
        throw file_write_error("File write error: " + get_path());
    }
}
