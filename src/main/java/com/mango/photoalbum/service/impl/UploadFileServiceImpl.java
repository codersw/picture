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
import com.mango.photoalbum.enums.OrderEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.UploadFileService;
import com.mango.photoalbum.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Value;
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

    @Resource
    private RocketMQTemplate template;

    @Value("${alibaba.mq.topic}")
    private String topic;

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
            oss.save(file.getInputStream(), ossFileName);
            //oss文件路径获取
            uploadFile.setFilePath(oss.getViewUrl(ossFileName));
            //ots保存文件信息
            ots.creatRow(uploadFile);
            log.info("文件保存成功:{}", uploadFile.toString());
            //如果是封面修改相册封面
            if (uploadFileCo.getIsCover().equals(IsCoverEnum.TRUE.getValue())) {
                setCover(UploadFile.builder()
                        .fileId(uploadFile.getFileId())
                        .albumId(uploadFile.getAlbumId())
                        .modifyUserId(uploadFile.getCreateUserId())
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
    public UploadFile saveV1(UploadFileCo uploadFileCo) {
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
            oss.save(file.getInputStream(), ossFileName);
            //oss文件路径获取
            uploadFile.setFilePath(oss.getViewUrl(ossFileName));
            //ots保存文件信息
            ots.creatRow(uploadFile);
            log.info("文件保存成功:{}", uploadFile.toString());
            //如果是封面修改相册封面
            setCover(UploadFile.builder()
                    .fileId(uploadFile.getFileId())
                    .albumId(uploadFile.getAlbumId())
                    .modifyUserId(uploadFile.getCreateUserId())
                    .IsCover(uploadFileCo.getIsCover())
                    .build());
            //发起图片识别
            template.convertAndSend(topic, uploadFile);
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
            Integer isCover = IsCoverEnum.FALSE.getValue();
            for(UploadFileCo uploadFileCo : uploadFileCos) {
                if (StringUtils.isEmpty(uploadFileCo.getAlbumId())) {
                    uploadFileCo.setAlbumId(uploadFileMultiCo.getAlbumId());
                }
                if (uploadFileCo.getUserId() == null) {
                    uploadFileCo.setUserId(uploadFileMultiCo.getUserId());
                }
                if (uploadFileCo.getIsCover().equals(IsCoverEnum.TRUE.getValue())) {
                    isCover = IsCoverEnum.TRUE.getValue();
                }
                result.add(save(uploadFileCo));
            }
            //如果没有封面修改封面
            if(isCover.equals(IsCoverEnum.FALSE.getValue())) {
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
            //更新最后上传时间
            ots.updataRow(PhotoAlbum.builder()
                    .albumId(uploadFileMultiCo.getAlbumId())
                    .modifyTime(new Date())
                    .modifyUserId(uploadFileMultiCo.getUserId())
                    .build());
        }
        return result;
    }

    @Override
    public List<UploadFile> saveV1(UploadFileMultiCo uploadFileMultiCo) {
        List<UploadFile> result = new ArrayList<>();
        List<UploadFileCo> uploadFileCos = uploadFileMultiCo.getUploadFileCos();
        if(uploadFileCos != null && uploadFileCos.size() > 0) {
            Integer isCover = IsCoverEnum.FALSE.getValue();
            for(UploadFileCo uploadFileCo : uploadFileCos) {
                if (StringUtils.isEmpty(uploadFileCo.getAlbumId())) {
                    uploadFileCo.setAlbumId(uploadFileMultiCo.getAlbumId());
                }
                if (uploadFileCo.getUserId() == null) {
                    uploadFileCo.setUserId(uploadFileMultiCo.getUserId());
                }
                if (uploadFileCo.getIsCover().equals(IsCoverEnum.TRUE.getValue())) {
                    isCover = IsCoverEnum.TRUE.getValue();
                }
                result.add(saveV1(uploadFileCo));
            }
            //如果没有封面修改封面
            if(isCover.equals(IsCoverEnum.FALSE.getValue())) {
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
            //更新最后上传时间
            ots.updataRow(PhotoAlbum.builder()
                    .albumId(uploadFileMultiCo.getAlbumId())
                    .modifyTime(new Date())
                    .modifyUserId(uploadFileMultiCo.getUserId())
                    .build());
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
        setCover(uploadFile);
    }

    @Override
    public void delete(String fileId) {
        ots.updataRow(UploadFile.builder()
                .fileId(fileId)
                .isDel(IsDelEnum.TRUE.getValue())
                .modifyTime(new Date())
                .build());
        //因数据延迟插入可能会导致设置封面没有变化
        // 所有手动判断下取不是要删除的封面以外的第一张
        UploadFile uploadFile = get(fileId);
        PhotoAlbum photoAlbum = ots.retrieveRow(PhotoAlbum.builder()
                .albumId(uploadFile.getAlbumId())
                .build());
        if(photoAlbum.getCover().equals(fileId)) {
            List<UploadFile> uploadFiles = list(UploadFileListCo.builder()
                    .albumId(photoAlbum.getAlbumId())
                    .pageIndex(1)
                    .pageSize(2)
                    .build());
            if(!uploadFiles.isEmpty()) {
                for(UploadFile file : uploadFiles) {
                    if(!file.getFileId().equals(fileId)) {
                        file.setIsCover(IsCoverEnum.TRUE.getValue());
                        setCover(file);
                        break;
                    }
                }
            }
        }
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
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime", SortOrder.ASC))));
        Integer pageSize = uploadFileListCo.getPageSize();
        Integer pageIndex = uploadFileListCo.getPageIndex();
        Integer total = uploadFileListCo.getTotal();
        if(!total.equals(0)) {
            query.setLimit(total);
        } else {
            Integer offset = (pageIndex - 1) * pageSize;
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
            if(!CommonUtils.isNullOrEmpty(rows)){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, UploadFile.class));
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
    public Integer totalV1(UploadFileListV1Co uploadFileListCo) {
        List<Query> mustQueries = new ArrayList<>();
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        mustQueries.add(termQuery);
        if(StringUtils.isNotEmpty(uploadFileListCo.getAlbumId())) {
            TermQuery termQuery1 = new TermQuery();
            termQuery1.setFieldName("albumId");
            termQuery1.setTerm(ColumnValue.fromString(uploadFileListCo.getAlbumId()));
            mustQueries.add(termQuery1);
        }
        if(!CommonUtils.isNullOrEmpty(uploadFileListCo.getUserId())) {
            MatchQuery matchQuery = new MatchQuery();
            matchQuery.setFieldName("persons");
            matchQuery.setText(uploadFileListCo.getUserId().toString());
            mustQueries.add(matchQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
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
    public List<UploadFile> listV1(UploadFileListV1Co uploadFileListV1Co) {
        List<UploadFile> result = new ArrayList<>();
        List<Query> mustQueries = new ArrayList<>();
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        mustQueries.add(termQuery);
        if(StringUtils.isNotEmpty(uploadFileListV1Co.getAlbumId())) {
            TermQuery termQuery1 = new TermQuery();
            termQuery1.setFieldName("albumId");
            termQuery1.setTerm(ColumnValue.fromString(uploadFileListV1Co.getAlbumId()));
            mustQueries.add(termQuery1);
        }
        if(!CommonUtils.isNullOrEmpty(uploadFileListV1Co.getUserId())) {
            MatchQuery matchQuery = new MatchQuery();
            matchQuery.setFieldName("persons");
            matchQuery.setText(uploadFileListV1Co.getUserId().toString());
            mustQueries.add(matchQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
        query.setQuery(boolQuery);
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime",
                Objects.requireNonNull(EnumUtils.getByValue(OrderEnum.class, uploadFileListV1Co.getOrder())).getName()))));
        Integer pageSize = uploadFileListV1Co.getPageSize();
        Integer pageIndex = uploadFileListV1Co.getPageIndex();
        Integer total = uploadFileListV1Co.getTotal();
        if(!total.equals(0)) {
            query.setLimit(total);
        } else {
            Integer offset = (pageIndex - 1) * pageSize;
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
            if(!CommonUtils.isNullOrEmpty(rows)){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, UploadFile.class));
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
    public Integer totalV2(UploadFileListV2Co uploadFileListV2Co) {
        List<Query> mustQueries = new ArrayList<>();
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        mustQueries.add(termQuery);
        if(StringUtils.isNotEmpty(uploadFileListV2Co.getAlbumId())) {
            TermQuery termQuery1 = new TermQuery();
            termQuery1.setFieldName("albumId");
            termQuery1.setTerm(ColumnValue.fromString(uploadFileListV2Co.getAlbumId()));
            mustQueries.add(termQuery1);
        }
        if(!CommonUtils.isNullOrEmpty(uploadFileListV2Co.getUserId())) {
            MatchQuery matchQuery = new MatchQuery();
            matchQuery.setFieldName("persons");
            matchQuery.setText(uploadFileListV2Co.getUserId().toString());
            mustQueries.add(matchQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
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
    public List<UploadFile> listV2(UploadFileListV2Co uploadFileListV2Co) {
        List<UploadFile> result = new ArrayList<>();
        List<Query> mustQueries = new ArrayList<>();
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        mustQueries.add(termQuery);
        if(StringUtils.isNotEmpty(uploadFileListV2Co.getAlbumId())) {
            TermQuery termQuery1 = new TermQuery();
            termQuery1.setFieldName("albumId");
            termQuery1.setTerm(ColumnValue.fromString(uploadFileListV2Co.getAlbumId()));
            mustQueries.add(termQuery1);
        }
        if(!CommonUtils.isNullOrEmpty(uploadFileListV2Co.getUserId())) {
            MatchQuery matchQuery = new MatchQuery();
            matchQuery.setFieldName("persons");
            matchQuery.setText(uploadFileListV2Co.getUserId().toString());
            mustQueries.add(matchQuery);
        }
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(mustQueries);
        query.setQuery(boolQuery);
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime",
                Objects.requireNonNull(EnumUtils.getByValue(OrderEnum.class, uploadFileListV2Co.getOrder())).getName()))));
        Integer pageSize = uploadFileListV2Co.getPageSize();
        Integer pageIndex = uploadFileListV2Co.getPageIndex();
        Integer total = uploadFileListV2Co.getTotal();
        if(!total.equals(0)) {
            query.setLimit(total);
        } else {
            Integer offset = (pageIndex - 1) * pageSize;
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
            if(!CommonUtils.isNullOrEmpty(rows)){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, UploadFile.class));
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
    public void download(String fileId, HttpServletResponse response) {
        UploadFile file = get(fileId);
        if(file != null) {
            String path = fileId + file.getFileType();
            if(!oss.exists(path)) {
                response.setHeader("Content-Disposition",
                        "attachment;filename=" + new String(file.getFileName().getBytes(), StandardCharsets.ISO_8859_1));
                try {
                    oss.load(path, response.getOutputStream());
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("下载文件出错：{}", e.getMessage());
                    throw new RuntimeException(e.getMessage());
                }
            } else {
                throw new RuntimeException("想要下载的文件不存在" + fileId);
            }
        } else {
            throw new RuntimeException("想要下载的文件不存在" + fileId);
        }
    }

    @Override
    public Integer totalFileFace(UploadFileFaceListCo uploadFileFaceListCo) {
        List<Query> mustQueries = new ArrayList<>();
        if(!CommonUtils.isNullOrEmpty(uploadFileFaceListCo.getUserId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("person");
            termQuery.setTerm(ColumnValue.fromString(uploadFileFaceListCo.getUserId().toString()));
            mustQueries.add(termQuery);
        }
        if(!StringUtils.isEmpty(uploadFileFaceListCo.getFileId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("fileId");
            termQuery.setTerm(ColumnValue.fromString(uploadFileFaceListCo.getFileId()));
            mustQueries.add(termQuery);
        }
        SearchQuery query = new SearchQuery();
        if(CollectionUtils.isNotEmpty(mustQueries)) {
            BoolQuery boolQuery = new BoolQuery();
            boolQuery.setMustQueries(mustQueries);
            query.setQuery(boolQuery);
        }
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFileFace.class))
                .indexName(ots.getTableName(UploadFileFace.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return 0;
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<UploadFileFace> listFileFace(UploadFileFaceListCo uploadFileFaceListCo) {
        List<UploadFileFace> result = new ArrayList<>();
        List<Query> mustQueries = new ArrayList<>();
        if(!CommonUtils.isNullOrEmpty(uploadFileFaceListCo.getUserId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("person");
            termQuery.setTerm(ColumnValue.fromString(uploadFileFaceListCo.getUserId().toString()));
            mustQueries.add(termQuery);
        }
        if(!StringUtils.isEmpty(uploadFileFaceListCo.getFileId())) {
            TermQuery termQuery = new TermQuery();
            termQuery.setFieldName("fileId");
            termQuery.setTerm(ColumnValue.fromString(uploadFileFaceListCo.getFileId()));
            mustQueries.add(termQuery);
        }
        SearchQuery query = new SearchQuery();
        if(CollectionUtils.isNotEmpty(mustQueries)) {
            BoolQuery boolQuery = new BoolQuery();
            boolQuery.setMustQueries(mustQueries);
            query.setQuery(boolQuery);
        }
        int offset = (uploadFileFaceListCo.getPageIndex() - 1) * uploadFileFaceListCo.getPageSize();
        query.setOffset(offset);
        query.setLimit(uploadFileFaceListCo.getPageSize());
        query.setGetTotalCount(false);// 设置返回总条数
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime",
                Objects.requireNonNull(EnumUtils.getByValue(OrderEnum.class, uploadFileFaceListCo.getOrder())).getName()))));
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFileFace.class))
                .indexName(ots.getTableName(UploadFileFace.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return result;
        try {
            List<Row> rows = searchResponse.getRows();
            if(!CommonUtils.isNullOrEmpty(rows)){
                for(Row row : rows) {
                    result.add(ots.formatRow(row, UploadFileFace.class));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            log.error("获取列表出错：{}", e.getMessage());
            throw new RuntimeException(e.getMessage());
        }
        return result;
    }

    /**
     * 修改相册封面
     * @param uploadFile
     */
    private void setCover(UploadFile uploadFile) {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .modifyUserId(uploadFile.getModifyUserId())
                .modifyTime(new Date())
                .albumId(uploadFile.getAlbumId())
                .build();
        //如果是封面修改相册封面
        if(uploadFile.getIsCover().equals(IsCoverEnum.TRUE.getValue())) {
            photoAlbum.setCover(uploadFile.getFileId());
        }
        ots.updataRow(photoAlbum);
        log.info("修改相册封面成功:{}", uploadFile.toString());
    }
}
