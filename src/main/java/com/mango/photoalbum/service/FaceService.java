package com.mango.photoalbum.service;

import com.mango.photoalbum.model.FaceInfo;
import com.mango.photoalbum.model.FaceInfoCo;
import com.mango.photoalbum.model.FaceInfoListCo;

import java.util.List;
import java.util.Map;

public interface FaceService {

    void save(FaceInfoCo faceInfoCo);

    Integer total(FaceInfoListCo faceInfoListCo);

    List<FaceInfo> list(FaceInfoListCo faceInfoListCo);

    Map listFace();
}
