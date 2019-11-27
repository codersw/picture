package com.mango.photoalbum.service.impl;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.Row;
import com.alicloud.openservices.tablestore.model.search.SearchQuery;
import com.alicloud.openservices.tablestore.model.search.SearchRequest;
import com.alicloud.openservices.tablestore.model.search.SearchResponse;
import com.alicloud.openservices.tablestore.model.search.query.Query;
import com.alicloud.openservices.tablestore.model.search.query.QueryBuilder;
import com.alicloud.openservices.tablestore.model.search.query.RangeQuery;
import com.alicloud.openservices.tablestore.model.search.query.TermQuery;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.UploadFileService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.FileUtils;
import com.mango.photoalbum.utils.OssUtils;
import com.mango.photoalbum.utils.OtsUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

@Service
public class UploadFileServiceImpl implements UploadFileService {

    @Resource
    private OtsUtils ots;

    @Resource
    private OssUtils oss;

    @Override
    public UploadFile save(UploadFileCo uploadFileCo) throws Exception{
        ots.creatTable(UploadFile.class);
        if(!CommonUtils.isNullOrEmpty(uploadFileCo.getFile())){
            if(!Objects.requireNonNull(uploadFileCo.getFile().getContentType()).contains("image")){
               throw new Exception("上传文件类型只可以是图片");
            }
            String fileType = FileUtils.getFileType(uploadFileCo.getFile().getOriginalFilename());
            if(StringUtils.isBlank(uploadFileCo.getFileId())){
                uploadFileCo.setFileId(CommonUtils.UUID());
            }
            String ossFileName = uploadFileCo.getFileId() + fileType;
            oss.save(uploadFileCo.getFile().getInputStream(), ossFileName);
            UploadFile uploadFile = UploadFile.builder()
                    .fileId(uploadFileCo.getFileId())
                    .fileName(uploadFileCo.getFile().getOriginalFilename())
                    .filePath(oss.getViewUrl(ossFileName))
                    .fileSize(uploadFileCo.getFile().getSize())
                    .fileType(fileType)
                    .createTime(new Date())
                    .modifyTime(new Date())
                    .remark(uploadFileCo.getRemark())
                    .userId(uploadFileCo.getUserId())
                    .isDel(IsDelEnum.FALSE.getValue())
                    .build();
            ots.creatRow(uploadFile);
            return uploadFile;
        }else{
            throw new Exception("图片不可以是空的");
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
        TermQuery termQuery = new TermQuery(); // 设置查询类型为RangeQuery
        termQuery.setFieldName("isDel");  // 设置针对哪个字段
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(
                        SearchQuery.newBuilder()
                                .query((QueryBuilder) termQuery)
                                .limit(0)   // 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
                                .getTotalCount(true) // 设置返回总条数
                                .build())
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<UploadFile> list(UploadFileListCo uploadFileListCo) {
        TermQuery termQuery = new TermQuery(); // 设置查询类型为RangeQuery
        termQuery.setFieldName("isDel");  // 设置针对哪个字段
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        int offset = (uploadFileListCo.getPageIndex() - 1) * uploadFileListCo.getPageSize();
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(
                        SearchQuery.newBuilder()
                                .offset(offset)
                                .query((QueryBuilder) termQuery)
                                .limit(uploadFileListCo.getPageSize())
                                .getTotalCount(true) // 设置返回总条数
                                .build())
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
}
