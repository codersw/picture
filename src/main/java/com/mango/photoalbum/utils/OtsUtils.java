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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
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
            //创建多元索引
            createSearchIndex(c);
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
            String tableName = getTableName(c);
            log.info("创建联合索引开始:表{}索引{}", tableName, tableName);
            CreateSearchIndexRequest request = new CreateSearchIndexRequest();
            request.setTableName(tableName);
            request.setIndexName(tableName);
            request.setIndexSchema(toIndex(c));
            client.createSearchIndex(request);
            log.info("创建联合成功:表{}索引{}", tableName, tableName);
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.info("创建联合失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("创建联合失败!请求失败详情：{}",e.getMessage());
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
        } catch (ClientException e) {
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
        } catch (ClientException | IllegalAccessException | InstantiationException e) {
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
                rows.forEach(row -> {
                    try {
                        result.add(formatRow(row, startT.getClass()));
                    } catch (IllegalAccessException | InstantiationException e) {
                        log.error("格式化数据失败!详情:{}", e.getMessage());
                        e.printStackTrace();
                    }
                });
                return result;
            }
        } catch (TableStoreException e) {
            e.printStackTrace();
            log.error("查找数据失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
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
        } catch (ClientException e) {
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
        } catch (ClientException e) {
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
                   PrimaryKeySchema primaryKeySchema = new PrimaryKeySchema(name, PrimaryKeyType.STRING);
                   if (type.equals("class java.lang.Integer")){
                       primaryKeySchema = new PrimaryKeySchema(name, PrimaryKeyType.INTEGER, PrimaryKeyOption.AUTO_INCREMENT);
                   }
                   primaryKeySchemas.add(primaryKeySchema);
               }
               if(annotation.annotationType() ==  OTSColumn.class){
                   OTSColumn column = field.getAnnotation(OTSColumn.class);
                   if(!StringUtils.isBlank(column.name())){
                       name = column.name();
                   }
                   DefinedColumnSchema definedColumnSchema = new DefinedColumnSchema(name, DefinedColumnType.STRING);
                   if (type.equals("class java.lang.Integer")){
                       definedColumnSchema = new DefinedColumnSchema(name, DefinedColumnType.INTEGER);
                   }
                   if (type.equals("class java.lang.Boolean")){
                       definedColumnSchema = new DefinedColumnSchema(name, DefinedColumnType.BOOLEAN);
                   }
                   if (type.equals("class java.lang.Double")){
                       definedColumnSchema = new DefinedColumnSchema(name, DefinedColumnType.DOUBLE);
                   }
                   definedColumnSchemas.add(definedColumnSchema);
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
    private <T> RowPutChange toRow(T t) {
        Class<T> c = (Class<T>) t.getClass();
        List<Field> fields = Arrays.asList(c.getDeclaredFields());
        PrimaryKeyBuilder primaryKeyBuilder = PrimaryKeyBuilder.createPrimaryKeyBuilder();
        Map<String, ColumnValue> columnValue = new HashMap<>();
        fields.forEach(field -> {
            field.setAccessible(true);
            List<Annotation> annotations = Arrays.asList(field.getAnnotations());// 获取自定义注解
            annotations.forEach(annotation -> {
                String name = field.getName(); // 获取属性的名字
                String type = field.getGenericType().toString(); // 获取属性的类型
                try {
                    Object value = field.get(t);//获取属性值
                    if(value != null){
                        if(annotation.annotationType() ==  OTSPrimaryKey.class){
                            OTSPrimaryKey pk = field.getAnnotation(OTSPrimaryKey.class);
                            if(!StringUtils.isBlank(pk.name())){
                                name = pk.name();
                            }
                            primaryKeyBuilder.addPrimaryKeyColumn(name, PrimaryKeyValue.fromString((String) value));
                            if (type.equals("class java.lang.Integer")){
                                primaryKeyBuilder.addPrimaryKeyColumn(name, PrimaryKeyValue.AUTO_INCREMENT);
                            }
                        }
                        if(annotation.annotationType() ==  OTSColumn.class){
                            OTSColumn column = field.getAnnotation(OTSColumn.class);
                            if(!StringUtils.isBlank(column.name())){
                                name = column.name();
                            }
                            if (type.equals("class java.lang.String")){
                                columnValue.put(name, ColumnValue.fromString((String) value));
                            }
                            if (type.equals("class java.lang.Integer")){
                                columnValue.put(name, ColumnValue.fromLong((Integer) value));
                            }
                            if (type.equals("class java.lang.Boolean")){
                                columnValue.put(name, ColumnValue.fromBoolean((Boolean) value));
                            }
                            if (type.equals("class java.util.Date")){
                                columnValue.put(name, ColumnValue.fromString(DateUtils.dateToStr((Date) value,"yyyy-MM-dd HH:mm:ss")));
                            }
                        }
                    }
                } catch (IllegalAccessException e) {
                    log.error("生成数据出错{}", e.getMessage());
                    e.printStackTrace();
                }
            });
        });
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
    private IndexSchema toIndex(Class<?> c) {
        IndexSchema indexSchema = new IndexSchema();
        List<FieldSchema> fieldSchemas = new ArrayList<>();
        List<Field> fields = Arrays.asList(c.getDeclaredFields());
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
                    //TODO FieldType.TEXT支持分词,而KEYWORD只能用前缀匹配
                    FieldSchema fieldSchema = new FieldSchema(name, FieldType.KEYWORD).setIndex(true).setEnableSortAndAgg(true);
                    if (type.equals("class java.lang.Integer")){
                        fieldSchema = new FieldSchema(name, FieldType.LONG).setIndex(true).setEnableSortAndAgg(true);
                    }
                    // 设置按照Timestamp这一列进行预排序, Timestamp这一列必须建立索引，并打开EnableSortAndAgg
                    indexSchema.setIndexSort(new Sort(
                            Collections.singletonList(new FieldSort(name, SortOrder.ASC))));
                    fieldSchemas.add(fieldSchema);
                }
                if(annotation.annotationType() ==  OTSColumn.class){
                    OTSColumn column = field.getAnnotation(OTSColumn.class);
                    if(!StringUtils.isBlank(column.name())){
                        name = column.name();
                    }
                    FieldSchema fieldSchema = new FieldSchema(name, FieldType.KEYWORD).setIndex(true).setEnableSortAndAgg(true);
                    if (type.equals("class java.lang.Integer")){
                        fieldSchema = new FieldSchema(name, FieldType.LONG).setIndex(true).setEnableSortAndAgg(true);
                    }
                    if (type.equals("class java.lang.Boolean")){
                        fieldSchema = new FieldSchema(name, FieldType.BOOLEAN).setIndex(true).setEnableSortAndAgg(true);
                    }
                    if (type.equals("class java.lang.Double")){
                        fieldSchema = new FieldSchema(name, FieldType.DOUBLE).setIndex(true).setEnableSortAndAgg(true);
                    }
                    fieldSchemas.add(fieldSchema);
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
    public <T> T formatRow(Row row, Class<?> c) throws IllegalAccessException, InstantiationException {
        T result = (T) c.newInstance();
        List<Field> fields = Arrays.asList(c.getDeclaredFields());
        fields.forEach(field -> {
            field.setAccessible(true);
            String name = field.getName(); // 获取属性的名字
            String type = field.getGenericType().toString(); // 获取属性的类型
            try{
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
                            String value = columnValue.asString();
                            field.set(result, DateUtils.strToDate(value, "yyyy-MM-dd HH:mm:ss"));
                        }
                    }
                }
            }catch (IllegalAccessException | ParseException e){
                log.error("格式化返回值异常{}", e.getMessage());
                e.printStackTrace();
            }
        });
        return result;
    }

    /**
     * 获取自定义表名字
     * @param c
     * @return
     */
    public String getTableName(Class<?> c){
        String name = c.getAnnotation(OTSClass.class).name();
        if(StringUtils.isBlank(name)){
            name = c.getName();
        }
        return name;
    }
}
