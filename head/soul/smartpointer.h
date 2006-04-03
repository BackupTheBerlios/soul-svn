/**************************************************************************
7                          smartpointer.h  -  description
                             -------------------
    begin                : Tue Apr 24 2001
    copyright            : (C) 2001 by Piwowarski Benjamin, Denoyer Ludovic
    email                : denoyer@poleia.lip6.fr, bpiwowar@poleia.lip6.fr
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef SMARTPOINTER_H
#define SMARTPOINTER_H

#include <iostream>
#include <stdexcept>
#include <assert.h>
#include <typeinfo>
#include "demangle.h"


/** This template class defines a smart pointer
   * This pointer delete the object whenever no class use the object.
  *@author Piwowarski Benjamin
  */

template <class T>
class SmartPointer {
protected:
    
    //! Shared informations
    struct Informations {
        T* pointer; //!< Pointer
        int attached; //!< Number of attached objects
        int detached; //!< Number of detached objects
    };
    
    //! Our informations
    Informations *informations;
    //! Pointer is attached ?
    bool isAttached;
    
    /** Decrements the counter, and destroy the object if necessary (ie if counter[0] == 0)
     */
    void decrements() {
        // If informations equals 0, pointer is null
        if (!informations)
            return;
        
        if (isAttached && (informations->attached>0)) {
            informations->attached--;
            // If we are the last attached object, delete the object
            if (informations->attached == 0) {
                //         std::cerr << "[SmartPointer] Deleting " << informations->pointer << std::endl;
                delete informations->pointer;
            }
            
        } else if (informations->detached>0)
            informations->detached--;
        else
            throw; // shoudln't be here !
        
        // If we are the last SmartPointer, destroy the informations
        if ((informations->attached + informations->detached) == 0) {
            delete informations;
            informations = 0;
        }
    }
    
    /** Decrements the counter, and returns but *do not destroy* the object if cleanup is necessary (ie if counter[0] == 0)
     */
    T * decrementsWithoutHarm() {
        T *toDelete = 0;
        
        // If informations equals 0, pointer is null
        if (!informations)
            return 0;
        
        if (isAttached && (informations->attached>0)) {
            informations->attached--;
            // If we are the last attached object, delete the object
            if (informations->attached == 0) {
                //         std::cerr << "[SmartPointer] Deleting " << informations->pointer << std::endl;
                toDelete = informations->pointer;
            }
            
        } else if (informations->detached>0)
            informations->detached--;
        else
            throw; // shoudln't be here !
        
        // If we are the last SmartPointer, destroy the informations
        if ((informations->attached + informations->detached) == 0) {
            delete informations;
            informations = 0;
        }
        return toDelete;
    }
    
public:
    
    //! \name Constructors and destructors
    /** Create a new SmartPointer with value 0
     */
SmartPointer() : informations(0) {  }
    
    /// \brief Null pointer
SmartPointer(int c) : informations(0) {  
    if (c != 0) throw std::runtime_error("Initialisation of a smart pointer with an integer != 0");
}
    
    /** Create a new SmartPointer with a pointer
     */
explicit SmartPointer(T* _pointer) : informations(0), isAttached(true) {
    if (_pointer) {
        informations = new Informations;
        informations->pointer = _pointer;
        informations->attached = 1;
        informations->detached = 0;
    }
}
    
    /** Copy constructor */
SmartPointer(const SmartPointer &other) : informations(other.informations), isAttached(other.isAttached)  {
    if (informations) {
        if (isAttached)
            informations->attached++;
        else
            informations->detached++;
    }
}
    
    //! To allow another SmartPointer to get our pointer/counter
    template <class U>
        friend class SmartPointer;
    
    //! Constructor with U which is a subclass of T
    template <class U>
        SmartPointer(const SmartPointer<U> &other) :
        isAttached(other.isAttached) {
            
            if (other.informations && dynamic_cast<T*>(other.informations->pointer)
               ) {
                   informations = (typename SmartPointer<T>::Informations*)other.informations;
                   if (isAttached)
                       informations->attached++;
                   else
                       informations->detached++;
               } else
                   informations = 0;
        }
    
    /** Returns the number of time this pointer is shared */
    size_t size(bool with_detached = false) const {
        if (!informations)
            return 0;
    return (with_detached ? informations->detached : 0) + informations->attached;
    }
    
    //! Detach this pointer (ie, keep the value of this pointer, but does not counter)
    void deconnect() {
        if (isAttached) {
            informations->detached++;
            decrements();
            isAttached = false;
        }
    }
    
    /** Detach this pointer
        The pointer will not be deleted
    */
    void detach() {
        if (informations)
            informations->attached++;
    }
    
    /** Re-attach this pointer
      */
    bool reconnect() {
        if (!isAttached)  {
            if (isValid()) {
                informations->attached++;
                informations->detached--;
                isAttached = true;
            } else
                *this = 0;
        }
        return isAttached; // success
    }
    
    
    //! Is the pointer not valid (return true if not)
    bool operator!() {
        return (informations==0) || (informations->attached==0);
    }
    
    //! Is the pointer valid ?
    bool isValid() const {
        return informations && informations->attached;
    }
    operator bool() const {
        return isValid();
    }
    
    /** Destroy the smart pointer and delete the pointed object if necessary
     */
    ~SmartPointer() {
        decrements();
    }
    
    
    //! \name Operators
    //@{
    
    /*! Equals
     */
    SmartPointer & operator = (const SmartPointer &other) {
        
        // (1) Other's informations is NULL
        if (other.informations == 0) {
            if (informations != 0) {
                decrements();
                informations = 0;
            }
            return *this;
        }
        
        
        // (2) Other's information is same pointer
        if (informations && (other.informations->pointer == informations->pointer)) {
            // If the other is attached and we are detached and vice/versa
            if (other.isAttached != isAttached) {
                if (other.isAttached)
                    other.informations->attached++;
                else
                    other.informations->detached++;
                decrements();
                isAttached = other.isAttached;
            }
            return *this;
        }
        
        // (3) Everything else
        
        
        T * toDelete = decrementsWithoutHarm();
        informations = other.informations;
        isAttached = other.isAttached;
        if (isAttached)
            informations->attached++;
        else
            informations->detached++;
        if (toDelete) delete toDelete;
        
        return *this;
    }
    
    
    /** Conversion (explicit in order to avoid implicit errors) */
    T* get
        () const {
            if (informations)
                return informations->pointer;
            return 0;
        }
    
    /// Dynamic cast conversion
    template <class U>
        U* get
        () const {
            if (informations)
                return dynamic_cast<U*>(informations->pointer);
            return 0;
        }
    
    
    //! Access
    T * operator ->() const {
        if (informations && informations->pointer)
            return informations->pointer;
        throw std::runtime_error(std::string("Null pointer (") + DEMANGLE(T) + ")" );
        
    }
    
    
    T & operator *() const {
        if (informations && informations->pointer)
            return *informations->pointer;
        throw  std::runtime_error(std::string("Null pointer (") + DEMANGLE(T) + ")" );
    }
    
    
    
    //@}
};


/// Output the smartpointer or its value if defined
template<class T> std::ostream &operator<<(std::ostream &out, const SmartPointer<T> &p) {
    if (p)
        out << *p;
    else
        out << "[null " << DEMANGLE(T) << " pointer]";
    return out;
}

//! A pointer that deallocates itself
template <class T>
class SimplePointer {
private:
    T *x;
    SimplePointer(const SimplePointer &);
    SimplePointer();
    SimplePointer & operator=(const SimplePointer &);
public:
inline SimplePointer(T *_x) : x(_x) {}
    inline ~SimplePointer() {
        if (x)
            delete x;
    }
    inline operator T*() {
        return x;
    }
    inline T * operator ->() const {
        if (!x)
            throw  std::runtime_error(std::string("Null pointer (") + DEMANGLE(T) + ")" );
        return x;
    }
    /** Detach a pointer
        Returns the old pointer and set this pointer to 0 (so the old pointer will not be deleted)
      */
    inline T * detach() {
        T * y = x;
        x = 0;
        return y;
    }
    
};

#endif



