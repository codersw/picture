package com.mango.photoalbum.service.impl;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.Row;
import com.alicloud.openservices.tablestore.model.search.SearchQuery;
import com.alicloud.openservices.tablestore.model.search.SearchRequest;
import com.alicloud.openservices.tablestore.model.search.SearchResponse;
import com.alicloud.openservices.tablestore.model.search.query.*;
import com.alicloud.openservices.tablestore.model.search.sort.FieldSort;
import com.alicloud.openservices.tablestore.model.search.sort.Sort;
import com.alicloud.openservices.tablestore.model.search.sort.SortOrder;
import com.mango.photoalbum.enums.IsCoverEnum;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.UploadFileService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.FileUtils;
import com.mango.photoalbum.utils.OssUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.awt.image.BufferedImage;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Service
@Slf4j
public class UploadFileServiceImpl implements UploadFileService {

    @Resource
    private OtsUtils ots;

    @Resource
    private OssUtils oss;

    @Override
    public UploadFile save(UploadFileCo uploadFileCo) {
        UploadFile uploadFile = UploadFile.builder()
                .fileId(uploadFileCo.getFileId())
                .createTime(new Date())
                .modifyTime(new Date())
                .albumId(uploadFileCo.getAlbumId())
                .remark(uploadFileCo.getRemark())
                .createUserId(uploadFileCo.getUserId())
                .modifyUserId(uploadFileCo.getUserId())
                .isDel(IsDelEnum.FALSE.getValue())
                .IsCover(uploadFileCo.getIsCover())
                .build();
        try {
            MultipartFile file = uploadFileCo.getFile();
            String fileName = file.getOriginalFilename();
            String fileType = FileUtils.getFileType(fileName);
            uploadFile.setFileType(fileType);
            uploadFile.setFileSize((int)file.getSize());
            uploadFile.setFileName(fileName);
            BufferedImage img = FileUtils.toImage(file);
            uploadFile.setHeight(img.getHeight());
            uploadFile.setWidth(img.getWidth());
            if(StringUtils.isBlank(uploadFileCo.getFileId())) {
                uploadFile.setFileId(CommonUtils.UUID());
            }
            String ossFileName = uploadFile.getFileId() + fileType;
            //oss上传图片
            oss.save(uploadFileCo.getFile().getInputStream(), ossFileName);
            //oss文件路径获取
            uploadFile.setFilePath(oss.getViewUrl(ossFileName));
            //ots保存文件信息
            ots.creatRow(uploadFile);
            log.info("文件保存成功:{}", uploadFile.toString());
            //如果是封面修改相册封面
            if (uploadFileCo.getIsCover().equals(IsCoverEnum.TRUE.getValue())) {
                setCover(UploadFile.builder()
                        .fileId(uploadFileCo.getFileId())
                        .albumId(uploadFileCo.getAlbumId())
                        .modifyUserId(uploadFileCo.getUserId())
                        .IsCover(IsCoverEnum.TRUE.getValue())
                        .build());
            }
            return uploadFile;
        } catch (Exception e) {
            e.printStackTrace();
            log.error("文件保存失败:{}", e.getMessage());
        }
        return uploadFile;
    }

    @Override
    public List<UploadFile> save(UploadFileMultiCo uploadFileMultiCo) {
        List<UploadFile> result = new ArrayList<>();
        List<UploadFileCo> uploadFileCos = uploadFileMultiCo.getUploadFileCos();
        if(uploadFileCos != null && uploadFileCos.size() > 0) {
            for(UploadFileCo uploadFileCo : uploadFileCos) {
                if (StringUtils.isEmpty(uploadFileCo.getAlbumId())) {
                    uploadFileCo.setAlbumId(uploadFileMultiCo.getAlbumId());
                }
                if (uploadFileCo.getUserId() == null) {
                    uploadFileCo.setUserId(uploadFileMultiCo.getUserId());
                }
                result.add(save(uploadFileCo));
            }
            //如果没有封面修改封面
            PhotoAlbum photoAlbum = ots.retrieveRow(PhotoAlbum.builder()
                    .albumId(uploadFileMultiCo.getAlbumId())
                    .build());
            if(photoAlbum != null && StringUtils.isEmpty(photoAlbum.getCover())) {
                setCover(UploadFile.builder()
                    .fileId(result.get(0).getFileId())
                    .albumId(result.get(0).getAlbumId())
                    .modifyUserId(result.get(0).getCreateUserId())
                    .IsCover(IsCoverEnum.TRUE.getValue())
                    .build());
            }
        }
        return result;
    }

    @Override
    public void update(UploadFileUpdateCo uploadFileUpdateCo) {
        UploadFile uploadFile = UploadFile.builder()
                .fileId(uploadFileUpdateCo.getFileId())
                .modifyTime(new Date())
                .modifyUserId(uploadFileUpdateCo.getUserId())
                .remark(uploadFileUpdateCo.getRemark())
                .albumId(uploadFileUpdateCo.getAlbumId())
                .IsCover(uploadFileUpdateCo.getIsCover())
                .build();
        ots.updataRow(uploadFile);
        //如果是封面修改相册封面
        setCover(uploadFile);
    }

    /**
     * 修改相册封面
     * @param uploadFile
     */
    private void setCover(UploadFile uploadFile) {
        ots.updataRow(PhotoAlbum.builder()
                .modifyUserId(uploadFile.getModifyUserId())
                .modifyTime(new Date())
                .albumId(uploadFile.getAlbumId())
                .cover(uploadFile.getFileId())
                .build());
        log.info("修改相册封面成功:{}", uploadFile.toString());
    }

    @Override
    public void delete(String fileId) {
        ots.updataRow(UploadFile.builder()
                .fileId(fileId)
                .isDel(IsDelEnum.TRUE.getValue())
                .modifyTime(new Date())
                .build());
    }

    @Override
    public UploadFile get(String fileId) {
        return ots.retrieveRow(UploadFile.builder()
                .fileId(fileId)
                .build());
    }

    @Override
    public Integer total(UploadFileListCo uploadFileListCo) {
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        TermQuery termQuery1 = new TermQuery();
        termQuery1.setFieldName("albumId");
        termQuery1.setTerm(ColumnValue.fromString(uploadFileListCo.getAlbumId()));
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(Arrays.asList(termQuery, termQuery1));
        query.setQuery(boolQuery);
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return 0;
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<UploadFile> list(UploadFileListCo uploadFileListCo) {
        List<UploadFile> result = new ArrayList<>();
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        TermQuery termQuery1 = new TermQuery();
        termQuery1.setFieldName("albumId");
        termQuery1.setTerm(ColumnValue.fromString(uploadFileListCo.getAlbumId()));
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(Arrays.asList(termQuery, termQuery1));
        query.setQuery(boolQuery);
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime", SortOrder.DESC))));
        Integer pageSize = uploadFileListCo.getPageSize();
        if(pageSize == 0) {
            query.setLimit(uploadFileListCo.getTotal());
        } else {
            int offset = (uploadFileListCo.getPageIndex() - 1) * pageSize;
            query.setOffset(offset);
            query.setLimit(pageSize);
        }
        query.setGetTotalCount(false);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return result;
        try {
            List<Row> rows = searchResponse.getRows();
            if(rows.size() > 0){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, UploadFile.class));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            log.error("获取列表报错：{}", e.getMessage());
        }
        return result;
    }

    @Override
    public void download(String fileId, HttpServletResponse response) throws Exception {
        UploadFile file = get(fileId);
        if(file != null) {
            String path = fileId + file.getFileType();
            if(!oss.exists(path)) {
                response.setHeader("Content-Disposition",
                        "attachment;filename=" + new String(file.getFileName().getBytes(), StandardCharsets.ISO_8859_1));
                oss.load(path, response.getOutputStream());
            } else {
                throw new RuntimeException("想要下载的文件不存在" + fileId);
            }
        } else {
            throw new RuntimeException("想要下载的文件不存在" + fileId);
        }
    }
}
