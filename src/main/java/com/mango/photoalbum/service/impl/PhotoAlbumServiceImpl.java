package com.mango.photoalbum.service.impl;

import com.alicloud.openservices.tablestore.model.Row;
import com.alicloud.openservices.tablestore.model.search.SearchQuery;
import com.alicloud.openservices.tablestore.model.search.SearchRequest;
import com.alicloud.openservices.tablestore.model.search.SearchResponse;
import com.alicloud.openservices.tablestore.model.search.agg.AggregationBuilders;
import com.alicloud.openservices.tablestore.model.search.query.Query;
import com.alicloud.openservices.tablestore.model.search.query.QueryBuilders;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.PhotoAlbumCo;
import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.model.PhotoAlbumListCo;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.OssUtils;
import com.mango.photoalbum.utils.OtsUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Service
public class PhotoAlbumServiceImpl implements PhotoAlbumService {

    @Resource
    private OtsUtils ots;

    @Resource
    private OssUtils oss;

    @Override
    public PhotoAlbum save(PhotoAlbumCo photoAlbumCo) {
        ots.creatTable(PhotoAlbum.class);
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId(photoAlbumCo.getAlbumId())
                .shootTime(photoAlbumCo.getShootTime())
                .userId(photoAlbumCo.getUserId())
                .title(photoAlbumCo.getTitle())
                .cover(photoAlbumCo.getCover())
                .shootLocation(photoAlbumCo.getShootLocation())
                .isDel(IsDelEnum.FALSE.getValue())
                .modifyTime(new Date())
                .createTime(new Date())
                .build();
        if(StringUtils.isBlank(photoAlbumCo.getAlbumId())){
            photoAlbum.setAlbumId(CommonUtils.UUID());
            ots.creatRow(photoAlbum);
        } else {
            ots.updataRow(photoAlbum);
        }
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
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(
                        SearchQuery.newBuilder()
                                .limit(0)   // 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
                                .getTotalCount(true) // 设置返回总条数
                                .build())
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<PhotoAlbum> list(PhotoAlbumListCo photoAlbumListCo) {
        int offset = (photoAlbumListCo.getPageIndex() - 1) * photoAlbumListCo.getPageSize();
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(
                        SearchQuery.newBuilder()
                                .offset(offset)
                                .limit(photoAlbumListCo.getPageSize())
                                .getTotalCount(true) // 设置返回总条数
                                .build())
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

}
