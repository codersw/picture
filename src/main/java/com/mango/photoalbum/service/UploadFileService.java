package com.mango.photoalbum.service;

import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.UploadFileCo;
import com.mango.photoalbum.model.UploadFileListCo;
import com.mango.photoalbum.model.UploadFileUpdateCo;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface UploadFileService {

    UploadFile save(UploadFileCo uploadFileCo) throws Exception;

    void update(UploadFileUpdateCo uploadFileUpdateCo);

    void delete(String fileId);

    UploadFile get(String fileId);

    Integer total(UploadFileListCo uploadFileListCo);

    List<UploadFile> list(UploadFileListCo uploadFileListCo);

    void download(String sourcePath, HttpServletResponse response) throws Exception;
}
