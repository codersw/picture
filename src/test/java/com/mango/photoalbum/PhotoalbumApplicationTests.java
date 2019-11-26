package com.mango.photoalbum;

import com.alicloud.openservices.tablestore.model.*;
import com.alicloud.openservices.tablestore.model.search.FieldType;
import com.alicloud.openservices.tablestore.model.search.SearchQuery;
import com.alicloud.openservices.tablestore.model.search.SearchRequest;
import com.alicloud.openservices.tablestore.model.search.SearchResponse;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.pojo.PhotoAlbum;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.OtsUtils;
import javafx.util.Pair;
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
                .userId(131312)
                .build();
        ots.creatRow(photoAlbum);
    }

    /**
     * 获取数据
     */
    @Test
    public void getRow() {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .build();
        photoAlbum = ots.retrieveRow(photoAlbum, PhotoAlbum.class);
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
        Map<String, FieldType> columnName = new HashMap<>();
        columnName.put("albumId",FieldType.LONG);
        ots.createSearchIndex(ots.getTableName(PhotoAlbum.class), "z_albumId", columnName);
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
    public void getRowsByPage() {
        String tableName = ots.getTableName(PhotoAlbum.class);
        int pageSize = 1;
        int offset = 0;
        PrimaryKey startKey = new PrimaryKey(new ArrayList<PrimaryKeyColumn>(){
            {
                add(new PrimaryKeyColumn("albumId", PrimaryKeyValue.INF_MIN));
            }
        });
        PrimaryKey endKey = new PrimaryKey(new ArrayList<PrimaryKeyColumn>(){
            {
                add(new PrimaryKeyColumn("albumId", PrimaryKeyValue.INF_MAX));
            }
        });
        // 读第一页，从范围的offset=33的行开始读起
        Pair<List<Row>, PrimaryKey> result = ots.readByPage(tableName, startKey, endKey, offset, pageSize);
        for (Row row : result.getKey()) {
            System.out.println(Arrays.toString(row.getColumns()));
        }
        System.out.println("Total rows count: " + result.getKey().size());
        // 顺序翻页，读完范围内的所有数据
        startKey = result.getValue();
        while (startKey != null) {
            result = ots.readByPage(tableName, startKey, endKey, 0, pageSize);
            for (Row row : result.getKey()) {
                System.out.println(Arrays.toString(row.getColumns()));
            }
            startKey = result.getValue();
            System.out.println("Total rows count: " + result.getKey().size());
        }
    }

    @Test
    public void rangeQuery(){
        String tableName = ots.getTableName(PhotoAlbum.class);
        ots.rangeQuery(tableName,"z_albumId");
    }
}
