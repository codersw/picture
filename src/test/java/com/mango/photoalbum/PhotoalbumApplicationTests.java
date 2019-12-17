package com.mango.photoalbum;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.search.*;
import com.alicloud.openservices.tablestore.model.search.query.BoolQuery;
import com.alicloud.openservices.tablestore.model.search.query.MatchPhraseQuery;
import com.alicloud.openservices.tablestore.model.search.query.TermQuery;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.FaceInfo;
import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.utils.FaceUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import java.util.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@Slf4j
public class PhotoalbumApplicationTests {

    @Autowired
    private OtsUtils ots;

    @Autowired
    private FaceUtils face;

    /**
     * 创建表格
     */
    @Test
    public void creatTable() {
        ots.creatTable(PhotoAlbum.class);
    }

    /**
     * 删除表格
     */
    @Test
    public void deleteTable() {
        ots.deleteTable(PhotoAlbum.class);
    }

    /**
     * 保存
     */
    @Test
    public void save() {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .cover("12313213")
                .createTime(new Date())
                .modifyTime(new Date())
                .shootTime(new Date())
                .shootLocation("asdasdasdasd")
                .isDel(IsDelEnum.FALSE.getValue())
                .title("sadasdd")
                .createUserId(131312)
                .modifyUserId(123444)
                .build();
        ots.creatRow(photoAlbum);
    }

    /**
     * 获取数据
     */
    @Test
    public void getRow() {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId("31fed80c0435448b983f1b28ea6f2d45")
                .build();
        photoAlbum = ots.retrieveRow(photoAlbum);
        log.info(photoAlbum.toString());
    }

    /**
     * 添加索引
     */
    @Test
    public void creatIndex() {
        ots.createIndex(ots.getTableName(PhotoAlbum.class), "z_albumId", "albumId");
    }

    @Test
    public void createSearchIndex() {
        ots.createSearchIndex(PhotoAlbum.class);
    }

    /**
     * 获取数据集合
     */
    @Test
    public void getRows() {
        List<PhotoAlbum> resultList = new ArrayList<>();
        SearchRequest request = new SearchRequest();
        request.setTableName(ots.getTableName(PhotoAlbum.class));
        request.setIndexName("z_albumId");
        SearchQuery searchQuery = SearchQuery.newBuilder().build();
        request.setSearchQuery(searchQuery);
        SearchResponse searchResponse = ots.searchQuery(request);
        log.info(searchResponse.toString());
    }

    @Test
    public void retrieveRow() {
        PhotoAlbum photoAlbum = ots.retrieveRow(PhotoAlbum.builder()
                .albumId("798c67eff8f94441ba64f822c965066b")
                .build());
        log.info(JSONObject.toJSONString(photoAlbum));
    }

    @Test
    public void addFace() {
        face.addFace(FaceInfo.builder()
                .Group("default")
                .Person("jackma")
                .Image("1")
                .ImageUrl("https://docs.alibabagroup.com/assets2/images/en/news/library_executives_jackma_large.jpg")
                .build());
    }

    @Test
    public void groupList() {
        log.info(JSON.toJSONString(face.listGroup()));
    }

    @Test
    public void recognizeFace() {
        face.recognizeFace("https://attach-mango.oss-cn-beijing.aliyuncs.com/lADPDgQ9q8L6V9vNByDNBVg_1368_1824.jpg", "default");
    }

    @Test
    public void listByFace() {
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        MatchPhraseQuery matchQuery1 = new MatchPhraseQuery();
        matchQuery1.setFieldName("persons");
        matchQuery1.setText("45087");
        SearchQuery query = new SearchQuery();
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(Arrays.asList(termQuery, matchQuery1));
        query.setQuery(boolQuery);
        query.setLimit(0);// 如果只关心统计聚合的结果，返回匹配到的结果数量设置为0有助于提高响应速度。
        query.setGetTotalCount(true);// 设置返回总条数
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        log.info(JSONObject.toJSONString(searchResponse));
    }
}
