# MapStruct Completion Examples

## Completion Menu Display

### Example 1: Simple Field Completion

When typing in a MapStruct annotation:

```java
@Mapper
public interface UserMapper {
    @Mapping(source = "user.address.|", target = "street")
    //                              ^ cursor here
    UserDTO toDto(User user);
}
```

The completion menu will show:

```
┌─────────────────────────────────────────────┐
│  street       String      Field      [MS]   │
│  city         String      Field      [MS]   │
│  zipCode      Integer     Field      [MS]   │
│ 󰊕 getCountry  String      Method     [MS]   │
└─────────────────────────────────────────────┘
```

### Example 2: Nested Object Completion

```java
@Mapper
public interface OrderMapper {
    @Mapping(source = "order.customer.address.|", target = "deliveryAddress")
    //                                        ^ cursor here
    OrderDTO toDto(Order order);
}
```

Completion menu:

```
┌──────────────────────────────────────────────────┐
│ 󰜢 street       String       Field       [MS]     │
│ 󰜢 city         String       Field       [MS]     │
│ 󰜢 state        String       Field       [MS]     │
│ 󰜢 zipCode      String       Field       [MS]     │
│ 󰜢 country      Country      Field       [MS]     │
│ 󰊕 getFullAddr  String       Method      [MS]     │
└──────────────────────────────────────────────────┘
```

### Example 3: Complex Type with Collections

```java
@Mapper
public interface ProductMapper {
    @Mapping(source = "product.category.|", target = "categoryName")
    //                               ^ cursor here
    ProductDTO toDto(Product product);
}
```

Completion menu showing nested type information:

```
┌──────────────────────────────────────────────────────┐
│ 󰜢 id           Long            Field       [MS]      │
│ 󰜢 name         String          Field       [MS]      │
│ 󰜢 description  String          Field       [MS]      │
│ 󰜢 parent       Category        Field       [MS]      │
│ 󰜢 children     List<Category>  Field       [MS]      │
│ 󰊕 getLevel     Integer         Method      [MS]      │
└──────────────────────────────────────────────────────┘
```

## Detailed Documentation on Hover

When you hover over a completion item (or press `<C-space>` in the menu), you'll see:

```
┌─────────────────────────────────────────────────┐
│ Field street                                    │
│ Type: java.lang.String                          │
│ Kind: FIELD                                     │
│ Source Class: Address                           │
│ Package: com.example.model                      │
│ Path: user.address.street                       │
└─────────────────────────────────────────────────┘
```

## Icon Legend

| Icon | Type     | Description                         |
| ---- | -------- | ----------------------------------- |
| 󰜢    | Field    | Direct field access                 |
| 󰊕    | Method   | Getter method (e.g., `getStreet()`) |
|      | Property | Generic property (fallback)         |

## Column Breakdown

The completion menu is organized into columns:

1. **Icon + Label Column**: Shows the icon and field/method name
2. **Type Column**: Shows the simplified type (e.g., "String", "Integer", "Category")
3. **Kind Column**: Shows "Field" or "Method"
4. **Source Column**: Shows `[MS]` to indicate MapStruct source

## Type Display

Types are intelligently simplified for readability:

| Full Type                   | Displayed As     |
| --------------------------- | ---------------- |
| `java.lang.String`          | `String`         |
| `java.lang.Integer`         | `Integer`        |
| `com.example.model.Address` | `Address`\*      |
| `java.util.List<String>`    | `List<String>`\* |

\* Non-java.lang types show the full package in hover documentation

## Field vs Getter Method

The system distinguishes between:

- **Fields**: Direct field access (e.g., `public String street;`)
    - Shows as "Field" in the Kind column
    - Uses field icon (󰜢)

- **Getter Methods**: Accessed via getter (e.g., `public String getStreet()`)
    - Shows as "Method" in the Kind column
    - Uses method icon (󰊕)
    - MapStruct automatically handles the getter-to-field mapping

Both will work correctly in your `@Mapping` annotations!

## Real-World Example

```java
package com.example.mapper;

import com.example.dto.UserDTO;
import com.example.model.User;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper
public interface UserMapper {

    // Example: Completing "user.profile."
    @Mapping(source = "user.profile.firstName", target = "name")
    //                            ^^^^^^^^^^^
    //                            Completions: firstName, lastName, getFullName(), etc.

    // Example: Completing "user.address."
    @Mapping(source = "user.address.street", target = "streetAddress")
    //                           ^^^^^^
    //                           Completions: street, city, zipCode, getFullAddress(), etc.

    // Example: Completing "user."
    @Mapping(source = "user.email", target = "contactEmail")
    //                     ^^^^^
    //                     Completions: email, username, profile, address, etc.

    UserDTO toDto(User user);
}
```

Each of these completion points will show:

- All available fields and methods
- Their types (String, Profile, Address, etc.)
- Whether they're fields or getters
- Full documentation on hover

## Performance

- **Fast**: Completions appear instantly via IPC communication and server implementation extremely lightweight and fast
- **Cached**: Results are cached per class/path for optimal performance
- **Reliable**: Heartbeat monitoring ensures clean server lifecycle
- **Automatic**: Server starts on first completion request

## Troubleshooting Display Issues

If completions don't show type information:

1. Verify Java server is returning data: `:MapStructStatus`
2. Test with ping: `:MapStructPing`
3. Check messages for errors: `:messages`
4. Ensure blink.cmp is configured with `label_description` column
5. Restart server: `:MapStructRestart`
