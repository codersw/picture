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
import com.mango.photoalbum.enums.IsPublicEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.text.ParseException;
import java.util.*;
import java.util.regex.Pattern;

@Service
@Slf4j
public class PhotoAlbumServiceImpl implements PhotoAlbumService {

    @Resource
    private OtsUtils ots;

    @Override
    public PhotoAlbum save(PhotoAlbumCo photoAlbumCo) {
        //TODO 无法校验标题
        // 原因：
        // 1.ots的字段索引一旦启用分词等就没有办法精确匹配了。
        // 2.ots数据插入延迟一旦连续插入数据没有办法校验。
        //checkTitle(photoAlbumCo.getTitle());
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId(photoAlbumCo.getAlbumId())
                .shootTime(photoAlbumCo.getShootTime())
                .modifyUserId(photoAlbumCo.getUserId())
                .title(photoAlbumCo.getTitle())
                .shootLocation(photoAlbumCo.getShootLocation())
                .isDel(IsDelEnum.FALSE.getValue())
                .modifyTime(new Date())
                .build();
        if(StringUtils.isBlank(photoAlbumCo.getAlbumId())){
            photoAlbum.setAlbumId(CommonUtils.UUID());
            photoAlbum.setCreateUserId(photoAlbumCo.getUserId());
            photoAlbum.setCreateTime(new Date());
            ots.creatRow(photoAlbum);
        } else {
            ots.updataRow(photoAlbum);
        }
        log.info("相册保存成功{}", photoAlbum.toString());
        return photoAlbum;
    }

    @Override
    public PhotoAlbum saveV1(PhotoAlbumV1Co photoAlbumV1Co) {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId(photoAlbumV1Co.getAlbumId())
                .shootTime(photoAlbumV1Co.getShootTime())
                .modifyUserId(photoAlbumV1Co.getUserId())
                .title(photoAlbumV1Co.getTitle())
                .shootLocation(photoAlbumV1Co.getShootLocation())
                .isDel(IsDelEnum.FALSE.getValue())
                .modifyTime(new Date())
                .orgId(photoAlbumV1Co.getOrgId())
                .isPublic(photoAlbumV1Co.getIsPublic())
                .build();
        if(StringUtils.isBlank(photoAlbumV1Co.getAlbumId())){
            photoAlbum.setAlbumId(CommonUtils.UUID());
            photoAlbum.setCreateUserId(photoAlbumV1Co.getUserId());
            photoAlbum.setCreateTime(new Date());
            ots.creatRow(photoAlbum);
        } else {
            ots.updataRow(photoAlbum);
        }
        log.info("相册保存成功{}", photoAlbum.toString());
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
        if(photoAlbum!= null && !StringUtils.isBlank(photoAlbum.getCover())){
            UploadFile uploadFile = ots.retrieveRow(UploadFile.builder()
                    .fileId(photoAlbum.getCover())
                    .build());
            if(uploadFile != null && uploadFile.getIsDel().equals(IsDelEnum.FALSE.getValue())){
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
        if(searchResponse == null) return 0;
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<PhotoAlbum> list(PhotoAlbumListCo photoAlbumListCo) {
        List<PhotoAlbum> result = new ArrayList<>();
        SearchQuery query = new SearchQuery();
        query.setQuery(formatQuery(photoAlbumListCo));
        Integer pageSize = photoAlbumListCo.getPageSize();
        int offset = (photoAlbumListCo.getPageIndex() - 1) * photoAlbumListCo.getPageSize();
        query.setOffset(offset);
        query.setLimit(pageSize);
        query.setGetTotalCount(false);// 设置返回总条数
        query.setSort(new Sort(Collections.singletonList(new FieldSort("modifyTime", SortOrder.DESC))));//排序
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return result;
        try {
            List<Row> rows = searchResponse.getRows();
            if(rows.size() > 0) {
                for(Row row : rows) {
                    PhotoAlbum photoAlbum = ots.formatRow(row, PhotoAlbum.class);
                    if(!StringUtils.isBlank(photoAlbum.getCover())){
                        UploadFile uploadFile = ots.retrieveRow(UploadFile.builder()
                                .fileId(photoAlbum.getCover())
                                .build());
                        if(uploadFile != null && uploadFile.getIsDel().equals(IsDelEnum.FALSE.getValue())){
                            photoAlbum.setCoverPath(uploadFile.getFilePath());
                        }
                    }
                    result.add(photoAlbum);
                }
            }
        } catch (IllegalAccessException | InstantiationException | ParseException e) {
            log.error("获取列表出错:{}", e.getMessage());
            e.printStackTrace();
        }
        return result;
    }

    @Override
    public Integer totalV1(PhotoAlbumListV1Co photoAlbumListV1Co) {
        SearchQuery query = new SearchQuery();
        query.setQuery(formatQueryV1(photoAlbumListV1Co));
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return 0;
        return (int) searchResponse.getTotalCount();
    }

    @Override
    public List<PhotoAlbum> listV1(PhotoAlbumListV1Co photoAlbumListV1Co) {
        List<PhotoAlbum> result = new ArrayList<>();
        SearchQuery query = new SearchQuery();
        query.setQuery(formatQueryV1(photoAlbumListV1Co));
        Integer pageSize = photoAlbumListV1Co.getPageSize();
        int offset = (photoAlbumListV1Co.getPageIndex() - 1) * photoAlbumListV1Co.getPageSize();
        query.setOffset(offset);
        query.setLimit(pageSize);
        query.setGetTotalCount(false);// 设置返回总条数
        query.setSort(new Sort(Collections.singletonList(new FieldSort("modifyTime", SortOrder.DESC))));//排序
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .returnAllColumns(true) // 设置返回所有列
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse == null) return result;
        try {
            List<Row> rows = searchResponse.getRows();
            if(rows.size() > 0) {
                for(Row row : rows) {
                    PhotoAlbum photoAlbum = ots.formatRow(row, PhotoAlbum.class);
                    if(!StringUtils.isBlank(photoAlbum.getCover())){
                        UploadFile uploadFile = ots.retrieveRow(UploadFile.builder()
                                .fileId(photoAlbum.getCover())
                                .build());
                        if(uploadFile != null && uploadFile.getIsDel().equals(IsDelEnum.FALSE.getValue())){
                            photoAlbum.setCoverPath(uploadFile.getFilePath());
                        }
                    }
                    result.add(photoAlbum);
                }
            }
        } catch (IllegalAccessException | InstantiationException | ParseException e) {
            log.error("获取列表出错:{}", e.getMessage());
            e.printStackTrace();
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
        TermQuery termQuery = new TermQuery(); // 设置查询类型为精确查找
        termQuery.setFieldName("isDel");  // 设置针对哪个字段
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        boolQuery.setMustQueries(Collections.singletonList(termQuery));
        List<Query> queries = new ArrayList<>();
        String keyword = photoAlbumListCo.getKeyword();
        if(!StringUtils.isBlank(keyword)){
            //条件1
            MatchPhraseQuery matchQuery1 = new MatchPhraseQuery();
            matchQuery1.setFieldName("title");
            matchQuery1.setText(keyword);
            queries.add(matchQuery1);
            //条件2
            MatchPhraseQuery  matchQuery2 = new MatchPhraseQuery();
            matchQuery2.setFieldName("shootLocation");
            matchQuery2.setText(keyword);
            queries.add(matchQuery2);
            //条件3
            if(Pattern.compile("[0-9]*").matcher(keyword).matches()) {
                TermQuery termQuery3 = new TermQuery();
                termQuery3.setFieldName("createUserId");
                termQuery3.setTerm(ColumnValue.fromString(keyword));
                queries.add(termQuery3);
            }
            boolQuery.setShouldQueries(queries);
            //最少匹配一个搜索条件
            boolQuery.setMinimumShouldMatch(1);
        }
        return boolQuery;
    }

    /**
     * 格式化查询条件
     * @param photoAlbumListV1Co
     * @return
     */
    private BoolQuery formatQueryV1(PhotoAlbumListV1Co photoAlbumListV1Co) {
        //第一个条件拼接
        // ((isDel=0 and isPublic=0) or (title like 'keyword')
        // or (shootLocation like 'keyword') or (createUserId='keyword'))
        BoolQuery boolQuery = new BoolQuery();
        //设置未删除
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        //设置公开的相册
        TermQuery termQuery1 = new TermQuery();
        termQuery1.setFieldName("isPublic");
        termQuery1.setTerm(ColumnValue.fromLong(IsPublicEnum.PUBLIC.getValue()));
        boolQuery.setMustQueries(Arrays.asList(termQuery, termQuery1));
        List<Query> queries = new ArrayList<>();
        String keyword = photoAlbumListV1Co.getKeyword();
        if(StringUtils.isNotEmpty(keyword)){
            //设置标题
            MatchPhraseQuery matchQuery1 = new MatchPhraseQuery();
            matchQuery1.setFieldName("title");
            matchQuery1.setText(keyword);
            queries.add(matchQuery1);
            //拍摄地点
            MatchPhraseQuery  matchQuery2 = new MatchPhraseQuery();
            matchQuery2.setFieldName("shootLocation");
            matchQuery2.setText(keyword);
            queries.add(matchQuery2);
            //设置创建人
            if(Pattern.compile("[0-9]*").matcher(keyword).matches()) {
                TermQuery termQuery4 = new TermQuery();
                termQuery4.setFieldName("createUserId");
                termQuery4.setTerm(ColumnValue.fromString(keyword));
                queries.add(termQuery4);
            }
            //最少匹配一个搜索条件
            boolQuery.setMinimumShouldMatch(1);
            boolQuery.setShouldQueries(queries);
        }
        //第二个条件拼接 (orgId=22 and isPublic=1)
        BoolQuery boolQuery1 = new BoolQuery();
        //设置部门id
        TermQuery termQuery2 = new TermQuery();
        termQuery2.setFieldName("orgId");
        termQuery2.setTerm(ColumnValue.fromLong(photoAlbumListV1Co.getOrgId()));
        //设置不公开的相册
        TermQuery termQuery3 = new TermQuery();
        termQuery3.setFieldName("isPublic");
        termQuery3.setTerm(ColumnValue.fromLong(IsPublicEnum.NOPUBLIC.getValue()));
        boolQuery1.setMustQueries(Arrays.asList(termQuery2, termQuery3));
        //合并条件 (第一个条件) or (第二个条件)
        List<Query> queries1 = new ArrayList<>();
        queries1.add(boolQuery);
        queries1.add(boolQuery1);
        BoolQuery boolQuery2 = new BoolQuery();
        boolQuery2.setShouldQueries(queries1);
        return boolQuery2;
    }

    /**
     * 校验标题是否被使用过
     * @param title
     * @return
     */
    private void checkTitle(String title) {
        SearchQuery query = new SearchQuery();
        //条件1
        TermQuery termQuery = new TermQuery(); // 设置查询类型为精确查找
        termQuery.setFieldName("isDel");  // 设置针对哪个字段
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        //条件2
        TermQuery termQuery1 = new TermQuery(); // 设置查询类型为精确查找
        termQuery1.setFieldName("title");  // 设置针对哪个字段
        termQuery1.setTerm(ColumnValue.fromString(title));
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(Arrays.asList(termQuery,termQuery1));
        query.setQuery(boolQuery);
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(PhotoAlbum.class))
                .indexName(ots.getTableName(PhotoAlbum.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        if(searchResponse != null) {
            if(searchResponse.getTotalCount() != 0) {
                throw new RuntimeException("标题已经被使用过了");
            }
        }
    }
}
