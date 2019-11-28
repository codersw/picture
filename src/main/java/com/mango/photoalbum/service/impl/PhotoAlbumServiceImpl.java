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
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.PhotoAlbumCo;
import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.model.PhotoAlbumListCo;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.*;

@Service
@Slf4j
public class PhotoAlbumServiceImpl implements PhotoAlbumService {

    @Resource
    private OtsUtils ots;

    @Override
    public PhotoAlbum save(PhotoAlbumCo photoAlbumCo) {
        ots.creatTable(PhotoAlbum.class);
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId(photoAlbumCo.getAlbumId())
                .shootTime(photoAlbumCo.getShootTime())
                .modifyUserId(photoAlbumCo.getUserId())
                .title(photoAlbumCo.getTitle())
                .cover(photoAlbumCo.getCover())
                .shootLocation(photoAlbumCo.getShootLocation())
                .isDel(IsDelEnum.FALSE.getValue())
                .modifyTime(new Date())
                .createTime(new Date())
                .build();
        if(StringUtils.isBlank(photoAlbumCo.getAlbumId())){
            photoAlbum.setAlbumId(CommonUtils.UUID());
            photoAlbum.setCreateUserId(photoAlbumCo.getUserId());
            ots.creatRow(photoAlbum);
        } else {
            ots.updataRow(photoAlbum);
        }
        log.info("相册保存成功fileId:{},CreateUserId:{},modifyUserId:{}", photoAlbum.getAlbumId(), photoAlbum.getCreateUserId(), photoAlbum.getModifyUserId());
        return photoAlbum;
    }

    @Override
    public void delete(String albumId) {
        ots.updataRow(PhotoAlbum.builder()
                .albumId(albumId)
                .isDel(IsDelEnum.TRUE.getValue())
                .modifyTime(new Date())
                .build());
    }

    @Override
    public PhotoAlbum get(String albumId) {
        PhotoAlbum photoAlbum = ots.retrieveRow(PhotoAlbum.builder()
                .albumId(albumId)
                .build());
        if(!StringUtils.isBlank(photoAlbum.getCover())){
            UploadFile uploadFile = ots.retrieveRow(UploadFile.builder()
                    .fileId(photoAlbum.getCover())
                    .build());
            if(uploadFile != null){
                photoAlbum.setCoverPath(uploadFile.getFilePath());
            }
        }
        return photoAlbum;
    }

    @Override
    public Integer total(PhotoAlbumListCo photoAlbumListCo) {
        SearchQuery query = new SearchQuery();
        query.setQuery(formatQuery(photoAlbumListCo));
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<PhotoAlbum> list(PhotoAlbumListCo photoAlbumListCo) {
        SearchQuery query = new SearchQuery();
        query.setQuery(formatQuery(photoAlbumListCo));
        Integer pageSize = photoAlbumListCo.getPageSize();
        int offset = (photoAlbumListCo.getPageIndex() - 1) * photoAlbumListCo.getPageSize();
        query.setOffset(offset);
        query.setLimit(pageSize);
        query.setGetTotalCount(true);// 设置返回总条数
        query.setSort(new Sort(Collections.singletonList(new FieldSort("createTime",SortOrder.DESC))));
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        List<Row> rows = searchResponse.getRows();
        List<PhotoAlbum> result = new ArrayList<>();
        if(rows.size() > 0){
            rows.forEach(row -> {
                try {
                    PhotoAlbum photoAlbum = ots.formatRow(row, PhotoAlbum.class);
                    if(!StringUtils.isBlank(photoAlbum.getCover())){
                        UploadFile uploadFile = ots.retrieveRow(UploadFile.builder()
                                .fileId(photoAlbum.getCover())
                                .build());
                        if(uploadFile != null){
                            photoAlbum.setCoverPath(uploadFile.getFilePath());
                        }
                    }
                    result.add(photoAlbum);
                } catch (IllegalAccessException | InstantiationException e) {
                    e.printStackTrace();
                }
            });
        }
        return result;
    }

    /**
     * 格式化查询条件
     * @param photoAlbumListCo
     * @return
     */
    private BoolQuery formatQuery(PhotoAlbumListCo photoAlbumListCo) {
        BoolQuery boolQuery = new BoolQuery();
        TermQuery termQuery = new TermQuery(); // 设置查询类型为RangeQuery
        termQuery.setFieldName("isDel");  // 设置针对哪个字段
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        boolQuery.setMustQueries(Collections.singletonList(termQuery));
        List<Query> queries = new ArrayList<>();
        String keyword = photoAlbumListCo.getKeyword();
        if(!StringUtils.isBlank(keyword)){
            //模糊匹配
            WildcardQuery wildcardQuery = new WildcardQuery();
            wildcardQuery.setFieldName("title");
            wildcardQuery.setValue(keyword + "*");
            WildcardQuery wildcardQuery2 = new WildcardQuery();
            wildcardQuery2.setFieldName("createTime");
            wildcardQuery2.setValue(keyword + "*");
            WildcardQuery wildcardQuery3 = new WildcardQuery();
            wildcardQuery3.setFieldName("shootTime");
            wildcardQuery3.setValue(keyword + "*");
            WildcardQuery wildcardQuery4 = new WildcardQuery();
            wildcardQuery4.setFieldName("shootLocation");
            wildcardQuery4.setValue(keyword + "*");
            queries.add(wildcardQuery);
            queries.add(wildcardQuery2);
            queries.add(wildcardQuery3);
            queries.add(wildcardQuery4);
            TermQuery termQuery1 = new TermQuery();
            termQuery1.setFieldName("createUserId");
            termQuery1.setTerm(ColumnValue.fromString(keyword));
            TermQuery termQuery2 = new TermQuery();
            termQuery2.setFieldName("modifyUserId");
            termQuery2.setTerm(ColumnValue.fromString(keyword));
            queries.add(termQuery1);
            queries.add(termQuery2);
            boolQuery.setMustQueries(Collections.singletonList(termQuery));
            boolQuery.setShouldQueries(queries);
            //最少匹配一个搜索条件
            boolQuery.setMinimumShouldMatch(1);
        }
        return boolQuery;
    }
}
