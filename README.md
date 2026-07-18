# Supermarket Queue Simulator

A Racket implementation of a supermarket checkout queue simulator. The project models multiple checkout counters, assigns customers based on queue load, handles counter delays, and simulates the evolution of the system through a sequence of requests.

## Features

- Checkout counter abstraction using Racket structures
- Dynamic queue management
- Automatic customer assignment to the least loaded counter
- Priority checkout for customers with a limited number of items
- Support for checkout delays
- Efficient minimum queue selection
- Both tail-recursive and stack-recursive implementations of the minimum search algorithm

---

## Overview

The simulator models four checkout counters, each maintaining:

- a unique identifier
- the total processing time (`tt`)
- the queue of waiting customers

The total processing time represents the sum of all items waiting to be processed together with any additional delay applied to the counter.

Customers are assigned to the checkout with the lowest total processing time. Customers purchasing more than the configured item limit are only assigned to regular checkout counters, while customers with fewer items may also use the express checkout.

---

## Request Types

The simulator processes requests sequentially.

### Customer Arrival

A request of the form

```text
(name number-of-items)
```

adds a new customer to the appropriate checkout queue.

### Counter Delay

A request of the form

```text
(delay counter-id minutes)
```

increases the processing time of the specified checkout, simulating temporary delays.

---

## Implemented Functionality

- Creation of empty checkout counters
- Processing time updates
- Customer insertion into queues
- Tail-recursive minimum processing time search
- Stack-recursive minimum processing time search
- Complete simulation of customer arrivals and checkout delays

---

## Technologies

- Racket
- Functional Programming
- Recursion
- Pattern Matching
- Immutable Data Structures
