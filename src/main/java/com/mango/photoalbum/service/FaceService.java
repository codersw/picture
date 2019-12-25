package com.mango.photoalbum.service;

import com.mango.photoalbum.model.*;

import java.util.List;
import java.util.Map;

public interface FaceService {

    void save(FaceInfoCo faceInfoCo);

    Integer total(FaceInfoListCo faceInfoListCo);

    List<FaceInfo> list(FaceInfoListCo faceInfoListCo);

    Map listFace();

    void handleFace(UploadFile uploadFile);

    FaceInfo getFace(Integer userId);
}
