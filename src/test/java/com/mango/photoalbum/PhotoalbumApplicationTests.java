package com.mango.photoalbum;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.search.*;
import com.alicloud.openservices.tablestore.model.search.query.BoolQuery;
import com.alicloud.openservices.tablestore.model.search.query.MatchQuery;
import com.alicloud.openservices.tablestore.model.search.query.TermQuery;
import com.mango.photoalbum.constant.FaceConstant;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.FaceInfo;
import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.UploadFileFace;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.FaceUtils;
import com.mango.photoalbum.utils.FileUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.acl.common.AclClientRPCHook;
import org.apache.rocketmq.acl.common.SessionCredentials;
import org.apache.rocketmq.client.AccessChannel;
import org.apache.rocketmq.client.exception.MQBrokerException;
import org.apache.rocketmq.client.exception.MQClientException;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.common.message.Message;
import org.apache.rocketmq.remoting.common.RemotingHelper;
import org.apache.rocketmq.remoting.exception.RemotingException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
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
                .albumId(CommonUtils.UUID())
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
        log.info(JSONObject.toJSONString(photoAlbum));
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
        log.info(JSONObject.toJSONString(photoAlbum));
    }

    /**
     * 添加索引
     */
    @Test
    public void creatIndex() {
        ots.createIndex(ots.getTableName(PhotoAlbum.class), "z_albumId", "albumId");
    }

    /**
     * 创建多元索引
     */
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

    /**
     * 获取详情
     */
    @Test
    public void retrieveRow() {
        PhotoAlbum photoAlbum = ots.retrieveRow(PhotoAlbum.builder()
                .albumId("798c67eff8f94441ba64f822c965066b")
                .build());
        log.info(JSONObject.toJSONString(photoAlbum));
    }

    /**
     * 添加到人脸库中
     */
    @Test
    public void addFace() {
        face.addFace(FaceInfo.builder()
                .group(FaceConstant.GROUP_DEFAUlT)
                .person("jackma")
                .image("1")
                .imageUrl("https://docs.alibabagroup.com/assets2/images/en/news/library_executives_jackma_large.jpg")
                .build());
    }

    /**
     * 获取库中的所有分组
     */
    @Test
    public void groupList() {
        List<String> group = face.listGroup();
        log.info(JSON.toJSONString(group));
    }

    /**
     * n：1 匹配人脸识别结果
     */
    @Test
    public void recognizeFace() {
        List<UploadFileFace> uploadFileFaces = face.recognizeFace(FaceInfo.builder()
                .imageUrl("https://attach-mango.oss-cn-beijing.aliyuncs.com/4039e7b9005243b1904fcbd30f437b03.jpg")
                .group(FaceConstant.GROUP_DEFAUlT)
                .build());
        log.info(JSONObject.toJSONString(uploadFileFaces));
    }

    /**
     * 获取有我的图片
     */
    @Test
    public void listByFace() {
        //条件1
        MatchQuery matchQuery = new MatchQuery();
        matchQuery.setFieldName("persons");
        matchQuery.setText("57564");
        //条件2
        TermQuery termQuery = new TermQuery();
        termQuery.setFieldName("isDel");
        termQuery.setTerm(ColumnValue.fromLong(IsDelEnum.FALSE.getValue()));
        //整合条件
        BoolQuery boolQuery = new BoolQuery();
        boolQuery.setMustQueries(Arrays.asList(matchQuery, termQuery));
        SearchQuery query = new SearchQuery();
        query.setQuery(boolQuery);
        query.setOffset(0);
        query.setLimit(10);
        query.setGetTotalCount(true);
        SearchRequest searchRequest = SearchRequest.newBuilder()
                .tableName(ots.getTableName(UploadFile.class))
                .indexName(ots.getTableName(UploadFile.class))
                .searchQuery(query)
                .build();
        SearchResponse searchResponse = ots.searchQuery(searchRequest);
        log.info(JSONObject.toJSONString(searchResponse));
    }

    @Test
    public void producer() throws InterruptedException, RemotingException, MQClientException, MQBrokerException, UnsupportedEncodingException {
        //设置为云上创建的 GID, 以及替换为自己的 AccessKeyId 和 AccessKeySecret
        DefaultMQProducer producer = new DefaultMQProducer("GID_default", new AclClientRPCHook(new SessionCredentials("LTAI4Fd66vvuVj31seQ2YbQQ","AgUkSMWtlICI7lY2oBFM0aD94ZlqYt")));
        //设置为自己的云上接入点
        producer.setNamesrvAddr("http://MQ_INST_1948640781844768_Bb2yJVY8.mq-internet-access.mq-internet.aliyuncs.com:80");
        // 云上消息轨迹需要设置为 CLOUD
        producer.setAccessChannel(AccessChannel.CLOUD);
        producer.start();
        // 设置为云上创建的 Topic 名字
        Message msg = new Message("topic_default", "Hello world".getBytes(RemotingHelper.DEFAULT_CHARSET));
        SendResult sendResult = producer.send(msg);
        log.info(JSONObject.toJSONString(sendResult));
        producer.shutdown();
    }

    @Test
    public void testClass() throws ClassNotFoundException {
        log.info("{}", ClassLoader.getSystemClassLoader().loadClass("com.mango.photoalbum.model.UploadFile"));
        log.info("{}", Class.forName("com.mango.photoalbum.model.UploadFile"));
    }

    @Test
    public void testImg() throws IOException {
        InputStream inputStream = FileUtils.openInputStream(new File("C:\\Users\\shaowen\\Desktop\\lADPDgQ9rF4QY9LNAcDNAcA_448_448.jpg"));
        log.info("{}", FileUtils.getImgInfo(inputStream));
    }
}
