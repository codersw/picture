package com.mango.photoalbum.utils;

import com.alicloud.openservices.tablestore.ClientException;
import com.alicloud.openservices.tablestore.SyncClient;
import com.alicloud.openservices.tablestore.TableStoreException;
import com.alicloud.openservices.tablestore.model.*;
import com.alicloud.openservices.tablestore.model.search.*;
import com.alicloud.openservices.tablestore.model.search.sort.FieldSort;
import com.alicloud.openservices.tablestore.model.search.sort.Sort;
import com.alicloud.openservices.tablestore.model.search.sort.SortOrder;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.*;

/**
 * ots工具类
 *@Author swen
 */
@Slf4j
@Component
public class OtsUtils {

    @Value("${alibaba.ots.accessKeyId}")
    private String accessKeyId;

    @Value("${alibaba.ots.accessKeySecret}")
    private String accessKeySecret;

    @Value("${alibaba.ots.endpoint}")
    private String endpoint;

    @Value("${alibaba.ots.instanceName}")
    private String instanceName ;

    private SyncClient client;

    /**
     * 初始化OTS访问对象
     */
    @PostConstruct
    private void init(){
        try {
            client = new SyncClient(endpoint, accessKeyId, accessKeySecret, instanceName);
        } catch (Exception e){
            client = null;
            e.printStackTrace();
            log.error("初始化ots出错:{}", e.getMessage());
        }
    }

    /**
     * 创建表格
     * @param c
     */
    public void creatTable(Class<?> c){
        try {
            if(existTable(c)) return;
            int timeToLive = -1; // 数据的过期时间, 单位秒, -1代表永不过期. 假如设置过期时间为一年, 即为 365 * 24 * 3600
            int maxVersions = 1; // 最大保存版本数, maxVersions大于1时, 无法使用二级索引和多元索引功能
            //描述表的配置信息
            TableOptions tableOptions = new TableOptions(timeToLive, maxVersions);
            //将表的结构信息和配置信息封装到一个request里
            CreateTableRequest request = new CreateTableRequest(toTable(c), tableOptions);
            //创建表格
            client.createTable(request);
            log.info("创建表格成功:{}", request.getTableMeta().getTableName());
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("创建表格失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("创建表格失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 删除表格
     * @param c
     */
    public void deleteTable(Class<?> c) {
        DeleteTableRequest request = new DeleteTableRequest(getTableName(c));
        try {
            client.deleteTable(request);
            log.info("删除表格成功:{}", request.getTableName());
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("删除表格失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("删除表格失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 判断表格是否存在
     * @param c
     * @return
     */
    private boolean existTable(Class<?> c) {
        try {
            String tableName = getTableName(c);
            log.info("判断表格是否存在开始:{}", tableName);
            DescribeTableRequest request = new DescribeTableRequest(tableName);
            DescribeTableResponse response = client.describeTable(request);
            TableMeta tableMeta = response.getTableMeta();
            if (tableMeta != null && tableMeta.getTableName() != null) {
                log.info("判断表格是否存在成功:{}", tableName);
                return true;
            }
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("判断表格是否存在失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("判断表格是否存在失败!请求失败详情：{}",e.getMessage());
        }
        return false;
    }

    /**
     * 创建索引
     * @param tableName
     * @param indexName
     * @param columnName
     */
    public void createIndex(String tableName, String indexName, String columnName) {

        try {
            log.info("创建索引开始:表{}索引{}列{}", tableName, indexName, columnName);
            IndexMeta indexMeta = new IndexMeta(indexName); // 要创建的索引表名称。
            indexMeta.addPrimaryKeyColumn(columnName); // 为索引表添加主键列。
            CreateIndexRequest request = new CreateIndexRequest(tableName, indexMeta, true);
            client.createIndex(request);
            log.info("创建索引成功:表{}索引{}列{}", tableName, indexName, columnName);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.info("创建索引失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("创建索引失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 删除索引
     * @param tableName
     * @param indexName
     */
    public void deleteIndex(String tableName, String indexName) {
        try {
            log.info("删除索引开始:表{}索引{}", tableName, indexName);
            DeleteIndexRequest request = new DeleteIndexRequest(tableName, indexName); // 要删除的索引表及主表名
            client.deleteIndex(request);
            log.info("删除索引成功:表{}索引{}", tableName, indexName);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.info("删除索引失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("删除索引失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 创建多元索引
     * @param c
     */
    public void createSearchIndex(Class<?> c){
        try {
            if(!getTableIndex(c)) return;
            String tableName = getTableName(c);
            log.info("创建多元索引开始:表{}索引{}", tableName, tableName);
            CreateSearchIndexRequest request = new CreateSearchIndexRequest();
            request.setTableName(tableName);
            request.setIndexName(tableName);
            request.setIndexSchema(toIndex(c));
            client.createSearchIndex(request);
            log.info("创建多元索引成功:表{}索引{}", tableName, tableName);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.info("创建多元索引失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("创建多元索引失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 删除联合索引
     * @param tableName
     * @param searchIndexName
     */
    public void deleteSearchIndex(String tableName, String searchIndexName) {
        try {
            log.info("删除联合索引开始:表{}索引{}", tableName, searchIndexName);
            DeleteSearchIndexRequest request = new DeleteSearchIndexRequest();
            request.setTableName(tableName);
            request.setIndexName(searchIndexName);
            client.deleteSearchIndex(request);
            log.info("删除联合成功:表{}索引{}", tableName, searchIndexName);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.info("删除联合失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("删除联合失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 添加数据
     * @param t
     */
    public <T> void creatRow(T t){
        try {
            PutRowRequest request = new PutRowRequest(toRow(t));
            client.putRow(request);
            log.info("添加数据成功{}", request.getRowChange().getTableName());
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("添加数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException | IllegalAccessException e) {
            e.printStackTrace();
            log.error("添加数据失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 通过主键获取数据
     * @param t
     * @return
     */
    public <T> T retrieveRow(T t) {
        try {
            RowPutChange row = toRow(t);
            //读取一行
            SingleRowQueryCriteria rowQueryCriteria = new SingleRowQueryCriteria(row.getTableName(), row.getPrimaryKey());
            //设置读取最新版本
            rowQueryCriteria.setMaxVersions(1);
            GetRowResponse getRowResponse = client.getRow(new GetRowRequest(rowQueryCriteria));
            log.info("查找数据成功{}", row.getTableName());
            Row responseRow = getRowResponse.getRow();
            if(responseRow != null){
                return formatRow(responseRow, t.getClass());
            }
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("查找数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException | IllegalAccessException | InstantiationException | ParseException e) {
            e.printStackTrace();
            log.error("查找数据失败!请求失败详情：{}",e.getMessage());
        }
        return null;
    }

    /**
     * 通过条件获取数据
     * @param startT
     * @param endT
     * @param limit
     * @param direction
     * @return
     */
    public <T> List<T> retrieveRow(T startT, T endT, Integer limit, Direction direction) {
        try {
            RowPutChange startRow = toRow(startT);
            RowPutChange entRow = toRow(endT);
            RangeRowQueryCriteria rangeRowQueryCriteria = new RangeRowQueryCriteria(startRow.getTableName());
            rangeRowQueryCriteria.setInclusiveStartPrimaryKey(startRow.getPrimaryKey());
            rangeRowQueryCriteria.setExclusiveEndPrimaryKey(entRow.getPrimaryKey());
            rangeRowQueryCriteria.setLimit(limit);
            rangeRowQueryCriteria.setDirection(direction);
            //设置读取最新版本
            rangeRowQueryCriteria.setMaxVersions(1);
            GetRangeResponse getRangeResponse = client.getRange(new GetRangeRequest(rangeRowQueryCriteria));
            log.info("查找数据成功{}", startRow.getTableName());
            List<Row> rows = getRangeResponse.getRows();
            if(!rows.isEmpty()){
                List<T> result = new ArrayList<>();
                for(Row row : rows) {
                    result.add(formatRow(row, startT.getClass()));
                }
                return result;
            }
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("查找数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException | IllegalAccessException | ParseException | InstantiationException e) {
            e.printStackTrace();
            log.error("查找数据失败!请求失败详情：{}",e.getMessage());
        }
        return null;
    }

    /**
     * 多条件查找数据
     * @param request
     */
    public SearchResponse searchQuery(SearchRequest request){
        try {
            log.info("多条件查找数据成功{}", request.getTableName());
            return client.search(request);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("多条件查找数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("多条件查找失败!请求失败详情：{}",e.getMessage());
        }
        return null;
    }

    /**
     * 更新数据
     * @param t
     */
    public <T> void updataRow(T t) {
        try {
            RowPutChange row = toRow(t);
            RowUpdateChange rowUpdateChange = new RowUpdateChange(row.getTableName(), row.getPrimaryKey());
            rowUpdateChange.put(row.getColumnsToPut());
            client.updateRow(new UpdateRowRequest(rowUpdateChange));
            log.info("更新数据成功{}", row.getTableName());
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("更新数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException | IllegalAccessException e) {
            e.printStackTrace();
            log.error("更新数据失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 删除数据
     * @param t
     */
    private <T> void deleteRow(T t) {
        try {
            RowPutChange row = toRow(t);
            RowDeleteChange rowDeleteChange = new RowDeleteChange(row.getTableName(), row.getPrimaryKey());
            client.deleteRow(new DeleteRowRequest(rowDeleteChange));
            log.info("删除数据成功{}", row.getTableName());
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("删除数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException | IllegalAccessException e) {
            log.error("删除数据失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 构造主键信息
     * @param primaryKeyValue
     * @return
     */
    private PrimaryKey creatPrimaryKey(Map<String, PrimaryKeyValue> primaryKeyValue){
        PrimaryKeyBuilder primaryKeyBuilder = PrimaryKeyBuilder.createPrimaryKeyBuilder();
        primaryKeyValue.keySet().forEach(key ->{
            primaryKeyBuilder.addPrimaryKeyColumn(key, primaryKeyValue.get(key));
        });
        return primaryKeyBuilder.build();
    }

    /**
     * 创建表格
     * @param c
     * @return
     */
    private TableMeta toTable(Class<?> c) {
        List<Field> fields = Arrays.asList(c.getDeclaredFields());
        List<PrimaryKeySchema> primaryKeySchemas = new ArrayList<>();
        List<DefinedColumnSchema> definedColumnSchemas = new ArrayList<>();
        fields.forEach(field -> {
            field.setAccessible(true);
            List<Annotation> annotations = Arrays.asList(field.getAnnotations());// 获取自定义注解
            annotations.forEach(annotation -> {
                String name = field.getName(); // 获取属性的名字
                String type = field.getGenericType().toString(); // 获取属性的类型
                if(annotation.annotationType() ==  OTSPrimaryKey.class){
                   OTSPrimaryKey pk = field.getAnnotation(OTSPrimaryKey.class);
                   if(!StringUtils.isBlank(pk.name())){
                       name = pk.name();
                   }
                   if(pk.primaryKeyAuto()) {
                       primaryKeySchemas.add(new PrimaryKeySchema(name, PrimaryKeyType.INTEGER, PrimaryKeyOption.AUTO_INCREMENT));
                   }
                   primaryKeySchemas.add(new PrimaryKeySchema(name, pk.primaryKeyType()));
               }
               if(annotation.annotationType() ==  OTSColumn.class){
                   OTSColumn column = field.getAnnotation(OTSColumn.class);
                   if(!StringUtils.isBlank(column.name())){
                       name = column.name();
                   }
                   definedColumnSchemas.add(new DefinedColumnSchema(name, column.definedColumnType()));
               }
            });
        });
        //描述表的结构信息
        TableMeta tableMeta = new TableMeta(getTableName(c));
        //添加主键列
        tableMeta.addPrimaryKeyColumns(primaryKeySchemas);
        //添加属性列
        tableMeta.addDefinedColumns(definedColumnSchemas);
        return tableMeta;
    }

    /**
     * 创建行数据
     * @param t
     * @param <T>
     * @return
     */
    private <T> RowPutChange toRow(T t) throws IllegalAccessException {
        Class<T> c = (Class<T>) t.getClass();
        Field[] fields = c.getDeclaredFields();
        PrimaryKeyBuilder primaryKeyBuilder = PrimaryKeyBuilder.createPrimaryKeyBuilder();
        Map<String, ColumnValue> columnValue = new HashMap<>();
        for(Field field : fields) {
            field.setAccessible(true);
            Annotation[] annotations = field.getAnnotations();// 获取自定义注解
            for(Annotation annotation : annotations) {
                String name = field.getName(); // 获取属性的名字
                String type = field.getGenericType().toString(); // 获取属性的类型
                Object value = field.get(t);//获取属性值
                if(value != null) {
                    if(annotation.annotationType() == OTSPrimaryKey.class){
                        OTSPrimaryKey pk = field.getAnnotation(OTSPrimaryKey.class);
                        if(!StringUtils.isBlank(pk.name())){
                            name = pk.name();
                        }
                        switch (pk.primaryKeyType()) {
                            case STRING:
                                primaryKeyBuilder.addPrimaryKeyColumn(name,
                                        PrimaryKeyValue.fromString((String) value));
                                break;
                            case INTEGER:
                                PrimaryKeyValue primaryKeyValue = PrimaryKeyValue.fromLong((Integer) value);
                                if(pk.primaryKeyAuto()) {
                                    primaryKeyValue = PrimaryKeyValue.AUTO_INCREMENT;
                                }
                                primaryKeyBuilder.addPrimaryKeyColumn(name, primaryKeyValue);
                                break;
                            case BINARY:
                                primaryKeyBuilder.addPrimaryKeyColumn(name,
                                        PrimaryKeyValue.fromBinary((byte[]) value));
                                break;
                        }
                    }
                    if(annotation.annotationType() == OTSColumn.class){
                        OTSColumn column = field.getAnnotation(OTSColumn.class);
                        if(!StringUtils.isBlank(column.name())){
                            name = column.name();
                        }
                        switch (column.definedColumnType()) {
                            case STRING:
                                assert value instanceof String;
                                columnValue.put(name, ColumnValue.fromString((String) value));
                                break;
                            case INTEGER:
                                if (type.equals("class java.util.Date")) {
                                    assert value instanceof Date;
                                    columnValue.put(name, ColumnValue.fromLong(((Date) value).getTime()));
                                } else {
                                    assert value instanceof Integer;
                                    columnValue.put(name, ColumnValue.fromLong((Integer) value));
                                }
                                break;
                            case BINARY:
                                assert value instanceof byte[];
                                columnValue.put(name, ColumnValue.fromBinary((byte[]) value));
                                break;
                            case DOUBLE:
                                assert value instanceof Double;
                                columnValue.put(name, ColumnValue.fromDouble((Double) value));
                                break;
                            case BOOLEAN:
                                assert value instanceof Boolean;
                                columnValue.put(name, ColumnValue.fromBoolean((Boolean) value));
                                break;
                        }
                    }
                }
            }
        }
        //属性列信息
        RowPutChange rowPutChange = new RowPutChange(getTableName(c), primaryKeyBuilder.build());
        columnValue.keySet().forEach(key ->{
            rowPutChange.addColumn(key, columnValue.get(key));
        });
        return rowPutChange;
    }

    /**
     * 构建多元索引
     * @param c
     * @return
     */
    private IndexSchema toIndex(Class<?> c) throws RuntimeException{
        IndexSchema indexSchema = new IndexSchema();
        List<FieldSchema> fieldSchemas = new ArrayList<>();
        List<Field> fields = Arrays.asList(c.getDeclaredFields());
        fields.forEach(field -> {
            field.setAccessible(true);
            List<Annotation> annotations = Arrays.asList(field.getAnnotations());// 获取自定义注解
            annotations.forEach(annotation -> {
                String name = field.getName(); // 获取属性的名字
                if(annotation.annotationType() ==  OTSPrimaryKey.class){
                    OTSPrimaryKey pk = field.getAnnotation(OTSPrimaryKey.class);
                    if(!StringUtils.isBlank(pk.name())){
                        name = pk.name();
                    }
                    switch (pk.indexType()) {
                        case NULL:
                            break;
                        case TEXT:
                            fieldSchemas.add(new FieldSchema(name, FieldType.TEXT).setStore(true).setIndex(true));
                            break;
                        case LONG:
                            fieldSchemas.add(new FieldSchema(name, FieldType.LONG).setIndex(true).setEnableSortAndAgg(true).setStore(true));
                            break;
                        case KEYWORD:
                            fieldSchemas.add(new FieldSchema(name, FieldType.KEYWORD).setStore(true).setIndex(true).setEnableSortAndAgg(true));
                            break;
                        default:
                            throw new RuntimeException("主键只允许TEXT,LONG,KEYWORD类型的多元索引");
                    }
                }
                if(annotation.annotationType() ==  OTSColumn.class){
                    OTSColumn column = field.getAnnotation(OTSColumn.class);
                    if(!StringUtils.isBlank(column.name())){
                        name = column.name();
                    }
                    switch (column.indexType()) {
                        case NULL:
                            break;
                        case TEXT:
                            fieldSchemas.add(new FieldSchema(name, FieldType.TEXT).setStore(true).setIndex(true));
                            break;
                        case LONG:
                            fieldSchemas.add(new FieldSchema(name, FieldType.LONG).setIndex(true).setEnableSortAndAgg(true).setStore(true));
                            if(name.toLowerCase().contains("Time")) {
                                indexSchema.setIndexSort(new Sort(
                                        Collections.singletonList(new FieldSort(name, SortOrder.DESC))));
                            }
                            break;
                        case KEYWORD:
                            fieldSchemas.add(new FieldSchema(name, FieldType.KEYWORD).setStore(true).setIndex(true).setEnableSortAndAgg(true));
                            break;
                        case DOUBLE:
                            fieldSchemas.add(new FieldSchema(name, FieldType.DOUBLE).setStore(true).setIndex(true).setEnableSortAndAgg(true));
                            break;
                        case BOOLEAN:
                            fieldSchemas.add(new FieldSchema(name, FieldType.BOOLEAN).setStore(true).setIndex(true).setEnableSortAndAgg(true));
                            break;
                        case NESTED:
                            fieldSchemas.add(new FieldSchema(name, FieldType.NESTED).setStore(true).setIndex(true));
                            break;
                        case GEO_POINT:
                            fieldSchemas.add(new FieldSchema(name, FieldType.GEO_POINT).setStore(true).setIndex(true));
                            break;
                    }

                }
            });
        });
        indexSchema.setFieldSchemas(fieldSchemas);
        return indexSchema;
    }

    /**
     * 格式化返回值
     * @param row
     * @param c
     * @return
     */
    public <T> T formatRow(Row row, Class<?> c) throws IllegalAccessException, InstantiationException, ParseException {
        T result = (T) c.newInstance();
        Field[] fields = c.getDeclaredFields();
        for(Field field : fields) {
            field.setAccessible(true);
            String name = field.getName(); // 获取属性的名字
            String type = field.getGenericType().toString(); // 获取属性的类型
            PrimaryKeyColumn primaryKeyColumn = row.getPrimaryKey().getPrimaryKeyColumnsMap().get(name);
            if(primaryKeyColumn != null) {
                if (type.equals("class java.lang.String")){
                    field.set(result, primaryKeyColumn.getValue().asString());
                }
                if (type.equals("class java.lang.Integer")){
                    long value = primaryKeyColumn.getValue().asLong();
                    field.set(result, (int) value);
                }
            }
            NavigableMap<Long, ColumnValue> columnMap = row.getColumnsMap().get(name);
            if(columnMap != null && !columnMap.isEmpty()) {
                ColumnValue columnValue = columnMap.firstEntry().getValue();
                if (columnValue != null) {
                    if (type.equals("class java.lang.String")){
                        field.set(result, columnValue.asString());
                    }
                    if (type.equals("class java.lang.Boolean")){
                        field.set(result, columnValue.asBoolean());
                    }
                    if (type.equals("class java.lang.Integer")){
                        long value = columnValue.asLong();
                        field.set(result, (int) value);
                    }
                    if (type.equals("class java.util.Date")){
                        long value = columnValue.asLong();
                        field.set(result, new Date(value));
                    }
                }
            }
        }
        return result;
    }

    /**
     * 获取自定义表名字
     * @param c
     * @return
     */
    public String getTableName(Class<?> c) {
        String name = c.getAnnotation(OTSClass.class).name();
        if(StringUtils.isBlank(name)){
            name = c.getName();
        }
        return name;
    }

    /**
     * 获取是否创建多元索引
     * @param c
     * @return
     */
    private boolean getTableIndex(Class<?> c) {
        return c.getAnnotation(OTSClass.class).searchIndex();
    }
}
