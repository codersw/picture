package com.mango.picture.utils;

import com.alicloud.openservices.tablestore.ClientException;
import com.alicloud.openservices.tablestore.SyncClient;
import com.alicloud.openservices.tablestore.TableStoreException;
import com.alicloud.openservices.tablestore.model.*;
import com.mango.picture.model.ots.TableStore;
import com.mango.picture.model.ots.TableStoreRow;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * ots工具类
 *@Author swen
 */
@Slf4j
@Data
@Component
public class OtsUtils {

    private static String accessKeyId;

    private static String accessKeySecret;

    private static String endpoint;

    private static String instanceName ;

    private static SyncClient client;

    @Value("${alibaba.ots.accessKeyId}")
    public void setAccessKeyId(String accessKeyId) {
        OtsUtils.accessKeyId = accessKeyId;
    }

    @Value("${alibaba.ots.accessKeySecret}")
    public void setAccessKeySecret(String accessKeySecret) {
        OtsUtils.accessKeySecret = accessKeySecret;
    }

    @Value("${alibaba.ots.endpoint}")
    public void setEndpoint(String endpoint) {
        OtsUtils.endpoint = endpoint;
    }

    @Value("${alibaba.ots.instanceName }")
    public void setInstanceName (String instanceName) {
        OtsUtils.instanceName  = instanceName ;
    }

    /**
     * 初始化OTS访问对象
     */
    static {
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
     * @param tableStore
     */
    public void creatTable(TableStore tableStore){
        log.info("开始创建表格{}", tableStore.getTableName());
        //描述表的结构信息
        TableMeta tableMeta = new TableMeta(tableStore.getTableName());
        //添加主键列
        tableMeta.addPrimaryKeyColumns(tableStore.getPrimaryKeySchemas());
        //添加属性列
        tableMeta.addDefinedColumns(tableStore.getDefinedColumnSchemas());
        int timeToLive = -1; // 数据的过期时间, 单位秒, -1代表永不过期. 假如设置过期时间为一年, 即为 365 * 24 * 3600
        int maxVersions = 1; // 最大保存版本数, maxVersions大于1时, 无法使用二级索引和多元索引功能
        //描述表的配置信息
        TableOptions tableOptions = new TableOptions(timeToLive, maxVersions);
        //将表的结构信息和配置信息封装到一个request里
        CreateTableRequest request = new CreateTableRequest(tableMeta, tableOptions);
        //创建表格
        try {
            client.createTable(request);
            log.info("创建表格成功:{}", tableStore.getTableName());
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
     * @param tableName
     */
    public void deleteTable(String tableName){
        log.info("删除表格开始:{}", tableName);
        DeleteTableRequest request = new DeleteTableRequest(tableName);
        try {
            client.deleteTable(request);
            log.info("删除表格成功:{}", tableName);
        } catch (TableStoreException e) {
            System.err.println("操作失败，详情：" + e.getMessage());
            log.error("删除表格失败!详情:{},Request ID:{}", e.getMessage(), e.getRequestId());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("删除表格失败!请求失败详情：{}",e.getMessage());
        }
    }

    /**
     * 判断表格是否存在
     * @param tableName
     * @return
     */
    public boolean existTable(String tableName) {
        log.info("判断表格是否存在开始:{}", tableName);
        DescribeTableRequest request = new DescribeTableRequest(tableName);
        try {
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
     * 添加数据
     * @param row
     */
    public void creatRow(TableStoreRow row){
        log.info("开始添加数据{}", row.getTableName());
        //主键信息
        PrimaryKey primaryKey = creatPrimaryKey(row.getPrimaryKeyValue());
        //属性列信息
        RowPutChange rowPutChange = new RowPutChange(row.getTableName(), primaryKey);
        Map<String, ColumnValue> columnValue = row.getColumnValue();
        columnValue.keySet().forEach(key ->{
            rowPutChange.addColumn(key, columnValue.get(key));
        });
        //添加新数据到表格
        try {
            client.putRow(new PutRowRequest(rowPutChange));
            log.info("添加数据成功{}", row.getTableName());
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
     * @param row
     * @return
     */
    public TableStoreRow retrieveRow(TableStoreRow row) {
        log.info("开始查找数据{}", row.getTableName());
        //主键信息
        PrimaryKey primaryKey = creatPrimaryKey(row.getPrimaryKeyValue());
        //读取一行
        SingleRowQueryCriteria rowQueryCriteria = new SingleRowQueryCriteria(row.getTableName(), primaryKey);
        //设置读取最新版本
        rowQueryCriteria.setMaxVersions(1);
        try {
            GetRowResponse getRowResponse = client.getRow(new GetRowRequest(rowQueryCriteria));
            Row responseRow = getRowResponse.getRow();
            if(responseRow == null) {
                log.info("查找数据结果集为空!");
                return null;
            }
            log.info("查找数据成功{}", row.getTableName());
            return formatTableStoreRow(responseRow);
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
     * 通过条件获取数据
     * @param startRow
     * @param endRow
     * @param limit
     * @param direction
     * @return
     */
    public List<TableStoreRow> retrieveRow(TableStoreRow startRow, TableStoreRow endRow, Integer limit, Direction direction) {
        log.info("开始查找数据{}", startRow.getTableName());
        //构建start主键
        PrimaryKey primaryStartKey = creatPrimaryKey(startRow.getPrimaryKeyValue());
        //构建end主键
        PrimaryKey primaryEndKey = creatPrimaryKey(endRow.getPrimaryKeyValue());
        RangeRowQueryCriteria rangeRowQueryCriteria = new RangeRowQueryCriteria(startRow.getTableName());
        rangeRowQueryCriteria.setInclusiveStartPrimaryKey(primaryStartKey);
        rangeRowQueryCriteria.setExclusiveEndPrimaryKey(primaryEndKey);
        rangeRowQueryCriteria.setLimit(limit);
        rangeRowQueryCriteria.setDirection(direction);
        //设置读取最新版本
        rangeRowQueryCriteria.setMaxVersions(1);
        try {
            GetRangeResponse getRangeResponse = client.getRange(new GetRangeRequest(rangeRowQueryCriteria));
            List<Row> rows = getRangeResponse.getRows();
            if(rows.isEmpty()) {
                log.info("查找数据结果集为空!");
                return null;
            }
            log.info("查找数据成功{}", startRow.getTableName());
            List<TableStoreRow> tableStoreRows = new ArrayList<>();
            rows.forEach(row ->{
                tableStoreRows.add(formatTableStoreRow(row));
            });
            return tableStoreRows;
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
     * 更新数据
     * @param row
     */
    public void updataRow(TableStoreRow row){
        //构建主键
        PrimaryKey primaryKey = creatPrimaryKey(row.getPrimaryKeyValue());
        RowUpdateChange rowUpdateChange = new RowUpdateChange(row.getTableName(), primaryKey);
        //构建属性列
        Map<String, ColumnValue> columnValue = row.getColumnValue();
        columnValue.keySet().forEach(key ->{
            rowUpdateChange.put(key, columnValue.get(key));
        });
        //添加新数据到表格
        try {
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
     * @param row
     */
    private void deleteRow(TableStoreRow row) {
        //构建主键
        PrimaryKey primaryKey = creatPrimaryKey(row.getPrimaryKeyValue());
        RowDeleteChange rowDeleteChange = new RowDeleteChange(row.getTableName(), primaryKey);
        //删除数据
        try {
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
     * 格式化单条数据
     * @param row
     * @return
     */
    private TableStoreRow formatTableStoreRow(Row row){
        //处理主键信息
        Map<String, PrimaryKeyValue> primaryKeyValueMap = new HashMap<>();
        Map<String, PrimaryKeyColumn> primaryKeyColumnsMap = row.getPrimaryKey().getPrimaryKeyColumnsMap();
        primaryKeyColumnsMap.keySet().forEach(key ->{
            primaryKeyValueMap.put(key, primaryKeyColumnsMap.get(key).getValue());
        });
        //处理属性列信息
        Map<String, ColumnValue> columnValueMap = new HashMap<>();
        List<Column> columns = Arrays.asList(row.getColumns());
        columns.forEach(column -> {
            columnValueMap.put(column.getName(), column.getValue());
        });
        return TableStoreRow.builder()
                .primaryKeyValue(primaryKeyValueMap)
                .columnValue(columnValueMap)
                .build();
    }
}
