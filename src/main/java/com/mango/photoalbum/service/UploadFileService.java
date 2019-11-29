package com.mango.photoalbum.service;

import com.mango.photoalbum.model.*;

import javax.servlet.http.HttpServletResponse;
import java.text.ParseException;
import java.util.List;

public interface UploadFileService {

    UploadFile save(UploadFileCo uploadFileCo) throws Exception;

    List<UploadFile> save(UploadFileMultiCo uploadFileMultiCo);

    void update(UploadFileUpdateCo uploadFileUpdateCo);

    void delete(String fileId);

    UploadFile get(String fileId);

    Integer total(UploadFileListCo uploadFileListCo);

    List<UploadFile> list(UploadFileListCo uploadFileListCo);

    void download(String sourcePath, HttpServletResponse response) throws Exception;
}
