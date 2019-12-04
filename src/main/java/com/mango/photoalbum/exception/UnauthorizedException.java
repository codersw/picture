package com.mango.photoalbum.exception;

/**
 * 未授权异常
 * @author swen
 */
public class UnauthorizedException extends RuntimeException {

    public UnauthorizedException(String message) {
        super(message);
    }
}
