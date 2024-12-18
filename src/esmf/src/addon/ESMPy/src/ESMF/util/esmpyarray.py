# $Id$

"""
The Array utility class
"""

#### IMPORT LIBRARIES #########################################################
import ESMF.api.constants as constants
from operator import mul
import numpy as np
import numpy.ma as ma
import ctypes as ct

def ndarray_from_esmf(data, dtype, shape):
    '''
    :param data: buffer of fortran allocated ESMF array
    :type data: ctypes void_p
    :param dtype: the type of the esmf buffer
    :type dtype: ESMF.TypeKind
    :param shape: N-D Python shape corresponding to 1D ESMF allocation
    :type shape: list or tuple
    :return: numpy array representing the data with dtype and shape
    '''
    # find the size of the local coordinates
    size = reduce(mul, shape)

    # create a numpy array to point to the ESMF data allocation
    buffer = np.core.multiarray.int_asbuffer(
        ct.addressof(data.contents),
        np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
    esmfarray = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

    esmfarray = esmfarray.reshape(shape, order='F')

    return esmfarray

class MaskedArray(ma.MaskedArray):

    def __new__(cls, data, mask, dtype, shape):
        '''
        :param cls: MaskedArray class type
        :param data: buffer of fortran allocated ESMF array
        :type data: ctypes void_p
        :param mask: mask corresponding to the fortran allocated ESMF array
        :type mask: list
        :param dtype: the type of the esmf buffer
        :type dtype: ESMF.TypeKind
        :param shape: N-D Python shape corresponding to 1D ESMF allocation
        :type shape: list or tuple
        :return: MaskedArray object

        :attribute contents: esmf array pointer
        '''
        # find the size of the local coordinates
        size = reduce(mul, shape)

        # create a numpy array to point to the ESMF data allocation
        buffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data.contents),
            np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
        npdata = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

        npdata = npdata.reshape(shape, order='F')

        if mask is None: mamask = [False]*size
        else: mamask = mask

        # create the new Field instance
        obj = super(MaskedArray, cls).__new__(cls, data=npdata, mask=mamask,
                                              dtype=constants._ESMF2PythonType[dtype])

        return obj

class Array(np.ndarray):
    """
    NOTE: this class has been proven to have some buggy behavior, use at your own risk!!
    """
    def __new__(cls, data, dtype, shape):
        '''
        :param cls: Array class type
        :param data: buffer of fortran allocated ESMF array
        :type data: ctypes void_p
        :param dtype: the type of the esmf buffer
        :type dtype: ESMF.TypeKind
        :param shape: N-D Python shape corresponding to 1D ESMF allocation
        :type shape: list or tuple
        :return: Array object

        :attribute contents: esmf array pointer
        '''
        # find the size of the local coordinates
        size = reduce(mul, shape)

        # create a numpy array to point to the ESMF data allocation
        buffer = np.core.multiarray.int_asbuffer(
            ct.addressof(data.contents),
            np.dtype(constants._ESMF2PythonType[dtype]).itemsize * size)
        npdata = np.frombuffer(buffer, constants._ESMF2PythonType[dtype])

        npdata = npdata.reshape(shape, order='F')

        # create the new Field instance
        obj = super(Array, cls).__new__(cls, tuple(shape),
                                         dtype=constants._ESMF2PythonType[dtype],
                                         buffer=npdata)

        # save objectwide metadata
        obj.contents = data.contents

        return obj

    def __array_finalize__(self, obj):
        if obj is None: return
        # set instance metadata
        self.contents = getattr(obj, 'contents', None)

    def __array_wrap__(self, out_arr):
        return np.ndarray.__array_wrap__(self, out_arr)
