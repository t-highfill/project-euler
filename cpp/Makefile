
CC = g++
CFLAGS = -g -Wall -Werror -O3
CPP11FLAGS = $(CFLAGS) -std=c++11
OMPFLAGS = -fopenmp

euler149: euler149.cpp
	$(CC) $(CFLAGS) -o euler149 euler149.cpp

euler209: euler209.cpp
	$(CC) $(CPP11FLAGS) -o euler209 euler209.cpp

euler265: euler265.cpp
	$(CC) $(CPP11FLAGS) -o euler265 euler265.cpp

euler216: euler216.cpp bitwiseset.cpp
	$(CC) $(CPP11FLAGS) -o euler216 euler216.cpp

euler301: euler301.cpp
	$(CC) $(CPP11FLAGS) $(OMPFLAGS) -o euler301 euler301.cpp

euler349: euler349.cpp
	$(CC) $(CPP11FLAGS) -o euler349 euler349.cpp

euler368: euler368.cpp
	$(CC) $(CPP11FLAGS) -o euler368 euler368.cpp

euler407: euler407.cpp
	$(CC) $(CPP11FLAGS) -pthread -o euler407 euler407.cpp
