package com.mango.photoalbum.service;

import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.UploadFileCo;
import com.mango.photoalbum.model.UploadFileListCo;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.util.List;

public interface UploadFileService {

    UploadFile save(UploadFileCo uploadFileCo) throws Exception;

    void update(UploadFileCo uploadFileCo);

    void delete(String fileId);

    UploadFile get(String fileId);

    Integer total(UploadFileListCo uploadFileListCo);

    List<UploadFile> list(UploadFileListCo uploadFileListCo);

    void download(String sourcePath, HttpServletResponse response) throws Exception;
}
