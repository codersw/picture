package com.mango.photoalbum.model;


import com.mango.photoalbum.enums.ResultCodeEnum;

/**
 * 接口返回值生成工具
 * @author swen
 */
public class ResultGenerator {

    /**
     * 返回成功
     **/
    private static final String DEFAULT_SUCCESS_MESSAGE = "SUCCESS";

    /**
     * 成功不返回参数
     * @return
     */
    public static Result genSuccessResult() {
        Result result = new Result();
        result.setCode(ResultCodeEnum.SUCCESS);
        result.setMessage(DEFAULT_SUCCESS_MESSAGE);
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
        result.setCode(ResultCodeEnum.SUCCESS);
        result.setMessage(DEFAULT_SUCCESS_MESSAGE);
        result.setData(data);
        return result;
    }

    /**
     * 失败不返回原因
     * @return
     */
    public static Result genFailResult() {
        Result result = new Result();
        result.setCode(ResultCodeEnum.FAIL);
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
        result.setCode(ResultCodeEnum.FAIL);
        result.setMessage(message);
        return result;
    }
}
