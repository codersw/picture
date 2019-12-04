package com.mango.photoalbum.model;


import com.mango.photoalbum.enums.ResultCodeEnum;

/**
 * 接口返回值生成工具
 * @author swen
 */
public class ResultGenerator {

    /**
     * 成功不返回参数
     * @return
     */
    public static Result genSuccessResult() {
        Result result = new Result();
        result.setCode(ResultCodeEnum.SUCCESS.getValue());
        result.setMessage(ResultCodeEnum.SUCCESS.getName());
        return result;
    }

    /**
     * 成功返回参数
     * @param data
     * @param <T>
     * @return
     */
    public static<T> Result<T> genSuccessResult(T data) {
        Result<T> result = new Result<T>();
        result.setCode(ResultCodeEnum.SUCCESS.getValue());
        result.setMessage(ResultCodeEnum.SUCCESS.getName());
        result.setData(data);
        return result;
    }

    /**
     * 失败不返回原因
     * @return
     */
    public static Result genFailResult() {
        Result result = new Result();
        result.setCode(ResultCodeEnum.FAIL.getValue());
        result.setMessage(ResultCodeEnum.FAIL.getName());
        return result;
    }

    /**
     * 失败返回原因
     * @param message
     * @return
     */
    public static Result<?> genFailResult(String message) {
        Result<?> result = new Result<>();
        result.setCode(ResultCodeEnum.FAIL.getValue());
        result.setMessage(message);
        return result;
    }
}
