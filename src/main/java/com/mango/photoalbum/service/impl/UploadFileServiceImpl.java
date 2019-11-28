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
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
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
        String fileType = FileUtils.getFileType(uploadFileCo.getFile().getOriginalFilename());
        UploadFile uploadFile = UploadFile.builder()
                .fileId(uploadFileCo.getFileId())
                .fileName(uploadFileCo.getFile().getOriginalFilename())
                .fileSize(uploadFileCo.getFile().getSize())
                .fileType(fileType)
                .createTime(new Date())
                .modifyTime(new Date())
                .albumId(uploadFileCo.getAlbumId())
                .remark(uploadFileCo.getRemark())
                .createUserId(uploadFileCo.getUserId())
                .modifyUserId(uploadFileCo.getUserId())
                .isDel(IsDelEnum.FALSE.getValue())
                .IsCover(uploadFileCo.getIsCover())
                .build();
        if(StringUtils.isBlank(uploadFileCo.getFileId())) {
            uploadFile.setFileId(CommonUtils.UUID());
        }
        String ossFileName = uploadFile.getFileId() + fileType;
        try {
            //oss上传图片
            oss.save(uploadFileCo.getFile().getInputStream(), ossFileName);
        } catch (Exception e) {
            e.printStackTrace();
            log.error(e.getMessage());
        }
        //oss文件路径获取
        uploadFile.setFilePath(oss.getViewUrl(ossFileName));
        //ots保存文件信息
        ots.creatRow(uploadFile);
        //如果是封面修改相册封面
        setCover(uploadFile);
        log.info("文件保存成功fileId:{},CreateUserId:{},modifyUserId:{}", uploadFile.getFileId(), uploadFile.getCreateUserId(), uploadFile.getModifyUserId());
        return uploadFile;
    }

    @Override
    public List<UploadFile> save(UploadFileMultiCo uploadFileMultiCo) {
        List<UploadFile> result = new ArrayList<>();
        List<UploadFileCo> uploadFileCos = uploadFileMultiCo.getUploadFileCos();
        if(uploadFileCos.size() > 0) {
            uploadFileCos.forEach(uploadFileCo -> {
                result.add(save(uploadFileCo));
            });
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
        if(uploadFile.getIsCover().equals(IsCoverEnum.TRUE.getValue()) && !StringUtils.isBlank(uploadFile.getAlbumId())){
            ots.updataRow(PhotoAlbum.builder()
                    .albumId(uploadFile.getAlbumId())
                    .cover(uploadFile.getFileId())
                    .build());
            log.info("修改相册封面成功fileId:{},malbumId:{},modifyUserId:{}", uploadFile.getFileId(), uploadFile.getAlbumId(), uploadFile.getModifyUserId());
        }
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
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<UploadFile> list(UploadFileListCo uploadFileListCo) {
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
        List<Row> rows = searchResponse.getRows();
        List<UploadFile> result = new ArrayList<>();
        if(rows.size() > 0){
            rows.forEach(row -> {
                try {
                    result.add(ots.formatRow(row, UploadFile.class));
                } catch (IllegalAccessException | InstantiationException e) {
                    e.printStackTrace();
                }
            });
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
