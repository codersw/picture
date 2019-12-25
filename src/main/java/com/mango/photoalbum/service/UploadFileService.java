package com.mango.photoalbum.service;

import com.mango.photoalbum.model.*;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.ParseException;
import java.util.List;

public interface UploadFileService {

    UploadFile save(UploadFileCo uploadFileCo);

    UploadFile saveV1(UploadFileCo uploadFileCo);

    List<UploadFile> save(UploadFileMultiCo uploadFileMultiCo);

    List<UploadFile> saveV1(UploadFileMultiCo uploadFileMultiCo);

    void update(UploadFileUpdateCo uploadFileUpdateCo);

    void delete(String fileId);

    UploadFile get(String fileId);

    Integer total(UploadFileListCo uploadFileListCo);

    List<UploadFile> list(UploadFileListCo uploadFileListCo);

    Integer totalV1(UploadFileListV1Co uploadFileListV1Co);

    List<UploadFile> listV1(UploadFileListV1Co uploadFileListV1Co);

    void download(String sourcePath, HttpServletResponse response);

    Integer totalFileFace(UploadFileFaceListCo uploadFileFaceListCo);

    List<UploadFileFace> listFileFace(UploadFileFaceListCo uploadFileFaceListCo);
}
