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

#ifndef __bin_file_h_
#define __bin_file_h_

#include <iostream>
#include <fstream>
#include <string>

#include "types.h"
#include "algorithms.h"


/** A class that facilitate write and read operations to a binary file. */
class bin_file : public std::fstream
{
public:

    //-------------------------------------------------------------------------
    // Constructors and open operations

    bin_file();

    /** Constructs and opens the file.
     *
     *  @param path The path of the file.
     *  @param mode The open mode.
     */
    bin_file(const char* path, const std::ios_base::openmode & mode);
    bin_file(const std::string & path, const std::ios_base::openmode & mode);

    /** Opens the file with the given parameters.
     *
     *  @param path The path of the file.
     *  @param mode The open mode.
     */
    void open(const char* path, const std::ios_base::openmode & mode);
    void open(const std::string& path, const std::ios_base::openmode & mode);


    //-------------------------------------------------------------------------
    // Static functions

    /** Returns the last modification date of the given file.
     *
     *  @param path The path of the file.
     *  @return The last modification date, in a string.
     */
    static std::string last_modification(const char* path);
    static std::string last_modification(std::string path);


    //-------------------------------------------------------------------------
    // Write operations

    /** Writes a string to the end of the file.
     *
     *  @param str The string to be written.
     *
     *  @exception file_write_error
     */
    void write(const std::string& str);
    
    /** Writes out the given object to the end of file.
     * 
     *  @param o The object to be written.
     *
     *  @exception file_write_error
     */
    template<class T>
    void write(const T& o) {
        check_writability();

        std::fstream::write((char*)&o, sizeof(T));
    }

    /** Writes out an object to the end of file, with the given size.
     *
     *  @param o Pointer to the object to be written.
     *  @param size The size of the object.
     *
     *  @exception file_write_error
     */
    template <class T, class U>
    void write(const T* o, const U& size) {
        check_writability();
        
        std::fstream::write((char*)o, size);
    }


    //-------------------------------------------------------------------------
    // Read operations

    /** Reads a string from the file.
     *
     *  @param str The variable into which the function reads.
     *
     *  @exception file_read_error
     */
    void read(std::string& str);

    /** Reads an object from the file.
     *
     *  @param o The variable into which the function reads the object.
     *
     *  @exception file_read_error
     */
    template <class T>
    void read(T& o) {
        check_readability();

        std::fstream::read((char*)&o, sizeof(T));
    }

    /** Reads an object from the file, that has the given size.
     *
     *  @param o A pointer which will points to the object.
     *  @param size Number of bytes to read.
     *
     *  @exception file_read_error
     */
    template <class T, class U>
    void read(T* o, const U& size) {
        check_readability();
        
        std::fstream::read((char*)o, size);
    }


    //-------------------------------------------------------------------------
    // Validation methods

    /** Writes the magic number to the file.
     *  The magic number is the size of the file, in bytes.
     */
    void write_magic_number();

    /** Reads the magic number form the file.
     *
     *  @return The magic number, which is the size of the file, in bytes.
     */
    size_type read_magic_number();

    /** Checks that the given file is not corrupted, so if the magic
     *  number (at the end of the file) is correct.
     *
     *  @return If the file is corrupted then returns true, otherwise false.
     */
    bool is_corrupted();

    /** Validates the file, and if there is something wrong with it's format,
     *  then throws a corrupted_file exception.
     *
     *  @exception corrupted_file
     */
    void validate();

    
    //-------------------------------------------------------------------------
    // Other functions

    /** Returns the last modification date.
     *
     *  @return The last modification date, in a string.
     */
    std::string get_last_modification() const;

    /** Returns the path of the file. */
    std::string get_path() const;

    /** Returns the size of file.
     *
     *  @return The size of file, in bytes.
     */
    size_type get_size();

    ~bin_file();

    
    //-------------------------------------------------------------------------
    // Exceptions

    /** Thrown during the validation process,
     *  if the file has not been opened before.
     */
    DEFINE_EXCEPTION(file_open_error, "Error while opening the file!")

    /** Thrown during the validation process,
     *  if the file's magic number does not correct.
     */
    DEFINE_EXCEPTION(corrupted_file, "Corrupted file!")

    /** Thrown when cannot read from the file. */
    DEFINE_EXCEPTION(file_read_error,  "File read error!")

    /** Thrown when cannot write to the file. */
    DEFINE_EXCEPTION(file_write_error, "File write error!")

private:

    //-------------------------------------------------------------------------
    // Properties

    /** Stores the path of the file. */
    std::string path;

    
    //-------------------------------------------------------------------------
    // Other private functions

    /** Every read function starts with that method.
     *
     *  @exception file_read_error.
     */
    void check_readability() const;

    /** Every write function starts with that method.
     *
     *  @exception file_write_error.
     */
    void check_writability() const;
    
};

#endif
