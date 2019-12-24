package com.mango.photoalbum.service.impl;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.Row;
import com.alicloud.openservices.tablestore.model.search.SearchQuery;
import com.alicloud.openservices.tablestore.model.search.SearchRequest;
import com.alicloud.openservices.tablestore.model.search.SearchResponse;
import com.alicloud.openservices.tablestore.model.search.query.BoolQuery;
import com.alicloud.openservices.tablestore.model.search.query.Query;
import com.alicloud.openservices.tablestore.model.search.query.TermQuery;
import com.alicloud.openservices.tablestore.model.search.sort.FieldSort;
import com.alicloud.openservices.tablestore.model.search.sort.Sort;
import com.alicloud.openservices.tablestore.model.search.sort.SortOrder;
import com.mango.photoalbum.constant.FaceConstant;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.FaceService;
import com.mango.photoalbum.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.awt.image.BufferedImage;
import java.util.*;

@Service
@Slf4j
public class FaceServiceImpl implements FaceService {

    @Resource
    private FaceUtils face;

    @Resource
    private OtsUtils ots;

    @Resource
    private OssUtils oss;

    /**
     * 获取消息
     */
//    private void getMessage() {
//        while (true) {
//            try {
//
//                if(!StringUtils.isEmpty(bodyStr)) {
//                    UploadFile file = JSONObject.parseObject(bodyStr, UploadFile.class);
//                    List<UploadFileFace> uploadFileFaces = face.recognizeFace(FaceInfo.builder()
//                            .imageUrl(file.getFilePath())
//                            .group(FaceConstant.GROUP_DEFAUlT)
//                            .build());
//                    if(!CommonUtils.isNullOrEmpty(uploadFileFaces)) {
//                        uploadFileFaces.forEach(uploadFileFace -> {
//                            uploadFileFace.setFileId(file.getFileId());
//                            ots.creatRow(uploadFileFace);
//                        });
//                        List<String> persons = uploadFileFaces.stream().filter(x -> x.getScore() >= FaceConstant.SCORE).map(UploadFileFace::getPerson).collect(Collectors.toList());
//                        ots.updataRow(UploadFile.builder()
//                                .fileId(file.getFileId())
//                                .persons(String.join(",", persons))
//                                .build());
//                    }
//                }
//                Thread.sleep(1000);
//            } catch (Exception e) {
//                e.printStackTrace();
//                log.error("MNS获取消息发生错误:{}" , e.getMessage());
//            }
//        }
//    }

    @Override
    public void save(FaceInfoCo faceInfoCo) {
        MultipartFile file = faceInfoCo.getFile();
        if(!CommonUtils.isNullOrEmpty(file)) {
            try {
                UploadFile uploadFile = UploadFile.builder()
                        .fileId(CommonUtils.UUID())
                        .createTime(new Date())
                        .modifyTime(new Date())
                        .albumId("")
                        .remark("")
                        .createUserId(faceInfoCo.getUserId())
                        .modifyUserId(faceInfoCo.getUserId())
                        .isDel(IsDelEnum.FALSE.getValue())
                        .persons(faceInfoCo.getUserId().toString())
                        .build();
                String fileName = file.getOriginalFilename();
                String fileType = FileUtils.getFileType(fileName);
                uploadFile.setFileType(fileType);
                uploadFile.setFileSize((int) file.getSize());
                uploadFile.setFileName(fileName);
                BufferedImage img = FileUtils.toImage(file);
                uploadFile.setHeight(img.getHeight());
                uploadFile.setWidth(img.getWidth());
                String ossFileName = uploadFile.getFileId() + fileType;
                //oss上传图片
                oss.save(file.getInputStream(), ossFileName);
                //oss文件路径获取
                uploadFile.setFilePath(oss.getViewUrl(ossFileName));
                //ots保存文件信息
                ots.creatRow(uploadFile);
                log.info("保存文件信息成功:{}", uploadFile.toString());
                FaceInfo faceInfo = FaceInfo.builder()
                        .faceId(CommonUtils.UUID())
                        .group(FaceConstant.GROUP_DEFAUlT)
                        .imageUrl(uploadFile.getFilePath())
                        .image(uploadFile.getCreateUserId().toString())
                        .person(uploadFile.getCreateUserId().toString())
                        .content("")
                        .fileId(uploadFile.getFileId())
                        .createTime(new Date())
                        .build();
                //face图片注册
                face.addFace(faceInfo);
                //ots保存人脸信息
                ots.creatRow(faceInfo);
                log.info("保存人脸信息成功:{}", faceInfo.toString());
            } catch (Exception e) {
                e.printStackTrace();
                log.error("保存人脸信息出错:{}", e.getMessage());
                throw new RuntimeException(e);
            }
        } else {
            throw new RuntimeException("图片不可以为空");
        }
    }

    @Override
    public Integer total(FaceInfoListCo faceInfoListCo) {
        List<Query> mustQueries = new ArrayList<>();
        if(!CommonUtils.isNullOrEmpty(faceInfoListCo.getUserId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("person");
            termQuery.setTerm(ColumnValue.fromString(faceInfoListCo.getUserId().toString()));
            mustQueries.add(termQuery);
        }
        if(!StringUtils.isEmpty(faceInfoListCo.getFileId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("fileId");
            termQuery.setTerm(ColumnValue.fromString(faceInfoListCo.getFileId()));
            mustQueries.add(termQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
        query.setQuery(boolQuery);
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(FaceInfo.class))
                .indexName(ots.getTableName(FaceInfo.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return 0;
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<FaceInfo> list(FaceInfoListCo faceInfoListCo) {
        List<FaceInfo> result = new ArrayList<>();
        List<Query> mustQueries = new ArrayList<>();
        if(!CommonUtils.isNullOrEmpty(faceInfoListCo.getUserId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("person");
            termQuery.setTerm(ColumnValue.fromString(faceInfoListCo.getUserId().toString()));
            mustQueries.add(termQuery);
        }
        if(!StringUtils.isEmpty(faceInfoListCo.getFileId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("fileId");
            termQuery.setTerm(ColumnValue.fromString(faceInfoListCo.getFileId()));
            mustQueries.add(termQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
        query.setQuery(boolQuery);
        int offset = (faceInfoListCo.getPageIndex() - 1) * faceInfoListCo.getPageSize();
        query.setOffset(offset);
        query.setLimit(faceInfoListCo.getPageSize());
        query.setGetTotalCount(false);// 设置返回总条数
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime", SortOrder.ASC))));
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(FaceInfo.class))
                .indexName(ots.getTableName(FaceInfo.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return result;
        try {
            List<Row> rows = searchResponse.getRows();
            if(!CommonUtils.isNullOrEmpty(rows)){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, FaceInfo.class));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            log.error("获取列表出错：{}", e.getMessage());
            throw new RuntimeException(e.getMessage());
        }
        return result;
    }

    @Override
    public Map listFace() {
        return face.listFace(FaceConstant.GROUP_DEFAUlT);
    }
}
