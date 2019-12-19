package com.mango.photoalbum.service;

import com.mango.photoalbum.model.FaceInfoCo;

public interface FaceService {

    void save(FaceInfoCo faceInfoCo);

    void delete(String Person, String Image);
}
